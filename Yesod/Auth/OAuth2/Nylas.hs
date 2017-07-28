{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.Nylas
    ( oauth2Nylas
    , module Yesod.Auth.OAuth2
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Monad (mzero)
import Control.Exception.Lifted (throwIO)
import Data.Aeson (FromJSON, Value(..), parseJSON, decode, (.:))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Client (applyBasicAuth, parseUrl, httpLbs, responseStatus
                           , responseBody)
import Network.HTTP.Conduit (Manager)
import Yesod.Auth (Creds(..), YesodAuth, AuthPlugin)
import Yesod.Auth.OAuth2 (OAuth2(..), AccessToken(..)
                         , YesodOAuth2Exception(InvalidProfileResponse)
                         , authOAuth2)
import qualified Network.HTTP.Types as HT

data NylasAccount = NylasAccount
    { nylasAccountId :: Text
    , nylasAccountEmailAddress :: Text
    , nylasAccountName :: Text
    , nylasAccountProvider :: Text
    , nylasAccountOrganizationUnit :: Text
    }

instance FromJSON NylasAccount where
    parseJSON (Object o) = NylasAccount
        <$> o .: "id"
        <*> o .: "email_address"
        <*> o .: "name"
        <*> o .: "provider"
        <*> o .: "organization_unit"
    parseJSON _ = mzero

oauth2Nylas :: YesodAuth m
            => Text -- ^ Client ID
            -> Text -- ^ Client Secret
            -> AuthPlugin m
oauth2Nylas clientId clientSecret = authOAuth2 "nylas" oauth fetchCreds
  where
    authorizeUrl = encodeUtf8 $ "https://api.nylas.com/oauth/authorize" <>
        "?response_type=code&scope=email&client_id=" <> clientId

    oauth = OAuth2
        { oauthClientId = encodeUtf8 clientId
        , oauthClientSecret = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint = authorizeUrl
        , oauthAccessTokenEndpoint = "https://api.nylas.com/oauth/token"
        , oauthCallback = Nothing
        }

fetchCreds :: Manager -> AccessToken -> IO (Creds a)
fetchCreds manager token = do
    req <- authorize <$> parseUrl "https://api.nylas.com/account"
    resp <- httpLbs req manager
    if HT.statusIsSuccessful (responseStatus resp)
        then case decode (responseBody resp) of
            Just ns -> return $ toCreds ns token
            Nothing -> throwIO parseFailure
        else throwIO requestFailure
  where
    authorize = applyBasicAuth (accessToken token) ""
    parseFailure = InvalidProfileResponse "nylas" "failed to parse account"
    requestFailure = InvalidProfileResponse "nylas" "failed to get account"

toCreds :: NylasAccount -> AccessToken -> Creds a
toCreds ns token = Creds
    { credsPlugin = "nylas"
    , credsIdent = nylasAccountId ns
    , credsExtra =
        [ ("email_address", nylasAccountEmailAddress ns)
        , ("name", nylasAccountName ns)
        , ("provider", nylasAccountProvider ns)
        , ("organization_unit", nylasAccountOrganizationUnit ns)
        , ("access_token", decodeUtf8 $ accessToken token)
        ]
    }
