{-# LANGUAGE OverloadedStrings #-}
-- |
-- OAuth2 plugin for <https://auth0.com>
--
-- * Authenticates against specific auth0 tenant
-- * Uses Auth0 user id (a.k.a [sub](https://auth0.com/docs/api/authentication#get-user-info)) as credentials identifier
--
module Yesod.Auth.OAuth2.Auth0
  ( oauth2Auth0HostScopes
  , oauth2Auth0Host
  , defaultAuth0Scopes
  ) where

import Data.Aeson as Aeson
import qualified Data.Text as T
import Prelude
import Yesod.Auth.OAuth2.Prelude

-- | https://auth0.com/docs/api/authentication#get-user-info
newtype User = User T.Text

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "sub"

-- | https://auth0.com/docs/get-started/apis/scopes/openid-connect-scopes#standard-claims
defaultAuth0Scopes :: [Text]
defaultAuth0Scopes = ["openid"]

pluginName :: Text
pluginName = "auth0"

oauth2Auth0Host :: YesodAuth m => URI -> Text -> Text -> AuthPlugin m
oauth2Auth0Host host = oauth2Auth0HostScopes host defaultAuth0Scopes

oauth2Auth0HostScopes
  :: YesodAuth m => URI -> [Text] -> Text -> Text -> AuthPlugin m
oauth2Auth0HostScopes host scopes clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (User uid, userResponse) <- authGetProfile pluginName
                                               manager
                                               token
                                               (host `withPath` "/userinfo")
    pure Creds { credsPlugin = pluginName
               , credsIdent  = uid
               , credsExtra  = setExtra token userResponse
               }
 where
  oauth2 = OAuth2
    { oauth2ClientId          = clientId
    , oauth2ClientSecret      = Just clientSecret
    , oauth2AuthorizeEndpoint = host
                                `withPath`  "/authorize"
                                `withQuery` [scopeParam " " scopes]
    , oauth2TokenEndpoint     = host `withPath` "/oauth/token"
    , oauth2RedirectUri       = Nothing
    }
