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
import Data.Vector ((!?))
import Network.HTTP.Client (applyBasicAuth, parseUrl, httpLbs, responseStatus
                           , responseBody)
import Network.HTTP.Conduit (Manager)
import Yesod.Auth (Creds(..), YesodAuth, AuthPlugin)
import Yesod.Auth.OAuth2 (OAuth2(..), AccessToken(..)
                         , YesodOAuth2Exception(InvalidProfileResponse)
                         , authOAuth2)

import qualified Data.Text as T
import qualified Network.HTTP.Types as HT

data NylasNamespace = NylasNamespace
    { nylasNamespaceId :: Text
    , nylasNamespaceAccountId :: Text
    , nylasNamespaceEmailAddress :: Text
    , nylasNamespaceName :: Text
    , nylasNamespaceProvider :: Text
    , nylasNamespaceOrganizationUnit :: Text
    }

instance FromJSON NylasNamespace where
    parseJSON (Array singleton) = case singleton !? 0 of
        Just (Object o) -> NylasNamespace
            <$> o .: "id"
            <*> o .: "account_id"
            <*> o .: "email_address"
            <*> o .: "name"
            <*> o .: "provider"
            <*> o .: "organization_unit"
        _ -> mzero
    parseJSON _ = mzero

oauth2Nylas :: YesodAuth m
            => Text -- ^ Client ID
            -> Text -- ^ Client Secret
            -> AuthPlugin m
oauth2Nylas = oauth2NylasScoped ["email"]

oauth2NylasScoped :: YesodAuth m
                  => [Text] -- ^ Scopes
                  -> Text   -- ^ Client ID
                  -> Text   -- ^ Client Secret
                  -> AuthPlugin m
oauth2NylasScoped scopes clientId clientSecret =
    authOAuth2 "nylas" oauth fetchCreds
  where
    authorizeUrl = encodeUtf8
                 $ "https://api.nylas.com/oauth/authorize?scope="
                 <> T.intercalate "," scopes
    tokenUrl = "https://api.nylas.com/oauth/token"
    oauth = OAuth2
        { oauthClientId = encodeUtf8 clientId
        , oauthClientSecret = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint = authorizeUrl
        , oauthAccessTokenEndpoint = tokenUrl
        , oauthCallback = Nothing
        }

fetchCreds :: Manager -> AccessToken -> IO (Creds a)
fetchCreds manager token = do
    req <- authorize <$> parseUrl "https://api.nylas.com/n"
    resp <- httpLbs req manager
    if HT.statusIsSuccessful (responseStatus resp)
        then case decode (responseBody resp) of
            Just ns -> return $ toCreds ns token
            Nothing -> throwIO parseFailure
        else throwIO requestFailure
  where
    authorize = applyBasicAuth (accessToken token) ""
    parseFailure = InvalidProfileResponse "nylas" "failed to parse namespace"
    requestFailure = InvalidProfileResponse "nylas" "failed to get namespace"

toCreds :: NylasNamespace -> AccessToken -> Creds a
toCreds ns token = Creds
    { credsPlugin = "nylas"
    , credsIdent = nylasNamespaceId ns
    , credsExtra =
        [ ("account_id", nylasNamespaceAccountId ns)
        , ("email_address", nylasNamespaceEmailAddress ns)
        , ("name", nylasNamespaceName ns)
        , ("provider", nylasNamespaceProvider ns)
        , ("organization_unit", nylasNamespaceOrganizationUnit ns)
        , ("access_token", decodeUtf8 $ accessToken token)
        ]
    }
