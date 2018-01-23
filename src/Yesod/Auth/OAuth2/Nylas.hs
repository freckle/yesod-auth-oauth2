{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.Nylas
    ( oauth2Nylas
    , module Yesod.Auth.OAuth2
    ) where

import Control.Exception.Lifted (throwIO)
import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value(..), decode, parseJSON, (.:))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
    (applyBasicAuth, httpLbs, parseRequest, responseBody, responseStatus)
import Network.HTTP.Conduit (Manager)
import qualified Network.HTTP.Types as HT
import Yesod.Auth (AuthPlugin, Creds(..), YesodAuth)
import Yesod.Auth.OAuth2

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
    oauth = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://api.nylas.com/oauth/authorize" `withQuery`
            [ ("response_type", "code")
            , ("scope", "email")
            , ("client_id", encodeUtf8 clientId)
            ]
        , oauthAccessTokenEndpoint = "https://api.nylas.com/oauth/token"
        , oauthCallback = Nothing
        }

fetchCreds :: Manager -> OAuth2Token -> IO (Creds a)
fetchCreds manager token = do
    req <- authorize <$> parseRequest "https://api.nylas.com/account"
    resp <- httpLbs req manager
    if HT.statusIsSuccessful (responseStatus resp)
        then case decode (responseBody resp) of
            Just ns -> return $ toCreds ns token
            Nothing -> throwIO parseFailure
        else throwIO requestFailure
  where
    authorize = applyBasicAuth (encodeUtf8 $ atoken $ accessToken token) ""
    parseFailure = InvalidProfileResponse "nylas" "failed to parse account"
    requestFailure = InvalidProfileResponse "nylas" "failed to get account"

toCreds :: NylasAccount -> OAuth2Token -> Creds a
toCreds ns token = Creds
    { credsPlugin = "nylas"
    , credsIdent = nylasAccountId ns
    , credsExtra =
        [ ("email_address", nylasAccountEmailAddress ns)
        , ("name", nylasAccountName ns)
        , ("provider", nylasAccountProvider ns)
        , ("organization_unit", nylasAccountOrganizationUnit ns)
        , ("access_token", atoken $ accessToken token)
        ]
    }
