{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.Nylas
    ( oauth2Nylas
    ) where

import Yesod.Auth.OAuth2.Prelude

import qualified Data.ByteString.Lazy.Char8 as BSL8
import Network.HTTP.Client
    (applyBasicAuth, httpLbs, parseRequest, responseBody, responseStatus)
import qualified Network.HTTP.Types as HT

data NylasAccount = NylasAccount
    { nylasAccountId :: Text
    , nylasAccountEmailAddress :: Text
    , nylasAccountName :: Text
    , nylasAccountProvider :: Text
    , nylasAccountOrganizationUnit :: Text
    }

instance FromJSON NylasAccount where
    parseJSON = withObject "NylasAccount" $ \o -> NylasAccount
        <$> o .: "id"
        <*> o .: "email_address"
        <*> o .: "name"
        <*> o .: "provider"
        <*> o .: "organization_unit"

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
        then case eitherDecode (responseBody resp) of
            Right ns -> return $ toCreds ns token
            Left err -> throwIO $ parseFailure err
        else throwIO requestFailure
  where
    authorize = applyBasicAuth (encodeUtf8 $ atoken $ accessToken token) ""
    parseFailure = InvalidProfileResponse "nylas" . BSL8.pack
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
