{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- |
--
-- OAuth2 plugin for http://login.salesforce.com
--
-- * Authenticates against Salesforce
-- * Uses Salesforce user id as credentials identifier
-- * Returns given_name, family_name, email and avatar_url as extras
--
module Yesod.Auth.OAuth2.Salesforce
    ( oauth2Salesforce
    , oauth2SalesforceScoped
    , oauth2SalesforceSandbox
    , oauth2SalesforceSandboxScoped
    , module Yesod.Auth.OAuth2
    ) where

import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Network.HTTP.Conduit (Manager)
import Yesod.Auth
import Yesod.Auth.OAuth2

oauth2Salesforce :: YesodAuth m
                 => Text -- ^ Client ID
                 -> Text -- ^ Client Secret
                 -> AuthPlugin m
oauth2Salesforce = oauth2SalesforceScoped ["openid", "email", "api"]

svcName :: Text
svcName = "salesforce"

oauth2SalesforceScoped :: YesodAuth m
                       => [Text] -- ^ List of scopes to request
                       -> Text -- ^ Client ID
                       -> Text -- ^ Client Secret
                       -> AuthPlugin m
oauth2SalesforceScoped scopes clientId clientSecret =
    authOAuth2 svcName oauth fetchSalesforceUser
  where
    oauth = OAuth2
        { oauthClientId            = clientId
        , oauthClientSecret        = clientSecret
        , oauthOAuthorizeEndpoint  = "https://login.salesforce.com/services/oauth2/authorize" `withQuery`
            [ scopeParam " " scopes
            ]
        , oauthAccessTokenEndpoint = "https://login.salesforce.com/services/oauth2/token"
        , oauthCallback            = Nothing
        }

fetchSalesforceUser :: Manager -> OAuth2Token -> IO (Creds m)
fetchSalesforceUser manager token = do
    result <- authGetJSON manager (accessToken token) "https://login.salesforce.com/services/oauth2/userinfo"
    case result of
        Right user -> return $ toCreds svcName user token
        Left err -> throwIO $ invalidProfileResponse svcName err

svcNameSb :: Text
svcNameSb = "salesforce-sandbox"

oauth2SalesforceSandbox :: YesodAuth m
                        => Text -- ^ Client ID
                        -> Text -- ^ Client Secret
                        -> AuthPlugin m
oauth2SalesforceSandbox = oauth2SalesforceSandboxScoped ["openid", "email"]


oauth2SalesforceSandboxScoped :: YesodAuth m
                              => [Text] -- ^ List of scopes to request
                              -> Text -- ^ Client ID
                              -> Text -- ^ Client Secret
                              -> AuthPlugin m
oauth2SalesforceSandboxScoped scopes clientId clientSecret =
    authOAuth2 svcNameSb oauth fetchSalesforceSandboxUser
  where
    oauth = OAuth2
        { oauthClientId            = clientId
        , oauthClientSecret        = clientSecret
        , oauthOAuthorizeEndpoint  = "https://test.salesforce.com/services/oauth2/authorize" `withQuery`
            [ scopeParam " " scopes
            ]
        , oauthAccessTokenEndpoint = "https://test.salesforce.com/services/oauth2/token"
        , oauthCallback            = Nothing
        }

fetchSalesforceSandboxUser :: Manager -> OAuth2Token -> IO (Creds m)
fetchSalesforceSandboxUser manager token = do
    result <- authGetJSON manager (accessToken token) "https://test.salesforce.com/services/oauth2/userinfo"
    case result of
        Right user -> return $ toCreds svcNameSb user token
        Left err -> throwIO $ invalidProfileResponse svcNameSb err

data User = User
    { userId :: Text
    , userOrg :: Text
    , userNickname :: Text
    , userName :: Text
    , userGivenName :: Text
    , userFamilyName :: Text
    , userTimeZone :: Text
    , userEmail :: Text
    , userPicture :: Text
    , userPhone :: Maybe Text
    , userRestUrl :: Text
    }

instance FromJSON User where
    parseJSON (Object o) = do
        userId          <- o .: "user_id"
        userOrg         <- o .: "organization_id"
        userNickname    <- o .: "nickname"
        userName        <- o .: "name"
        userGivenName   <- o .: "given_name"
        userFamilyName  <- o .: "family_name"
        userTimeZone    <- o .: "zoneinfo"
        userEmail       <- o .: "email"
        userPicture     <- o .: "picture"
        userPhone       <- o .:? "phone_number"
        urls            <- o .: "urls"
        userRestUrl     <- urls .: "rest"
        return User{..}

    parseJSON _ = mzero

toCreds :: Text -> User -> OAuth2Token -> Creds m
toCreds name user token = Creds
    { credsPlugin = name
    , credsIdent = userId user
    , credsExtra =
        [ ("email", userEmail user)
        , ("org", userOrg user)
        , ("nickname", userName user)
        , ("name", userName user)
        , ("given_name", userGivenName user)
        , ("family_name", userFamilyName user)
        , ("time_zone", userTimeZone user)
        , ("avatar_url", userPicture user)
        , ("rest_url", userRestUrl user)
        , ("access_token", atoken $ accessToken token)
        ]
        ++ maybeExtra "refresh_token" (rtoken <$> refreshToken token)
        ++ maybeExtra "expires_in" ((T.pack . show) <$> expiresIn token)
        ++ maybeExtra "phone_number" (userPhone user)
    }
