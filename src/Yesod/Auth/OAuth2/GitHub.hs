{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://github.com
--
-- * Authenticates against github
-- * Uses github user id as credentials identifier
--
module Yesod.Auth.OAuth2.GitHub
  ( oauth2GitHub
  , oauth2GitHubScoped
  ) where

import           Yesod.Auth.OAuth2.Prelude

import qualified Data.Text                     as T

newtype User = User Int

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "id"

pluginName :: Text
pluginName = "github"

defaultScopes :: [Text]
defaultScopes = ["user:email"]

oauth2GitHub :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2GitHub = oauth2GitHubScoped defaultScopes

oauth2GitHubScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2GitHubScoped scopes clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <- authGetProfile
      pluginName
      manager
      token
      "https://api.github.com/user"

    pure Creds { credsPlugin = pluginName
               , credsIdent  = T.pack $ show userId
               , credsExtra  = setExtra token userResponse
               }
 where
  oauth2 = OAuth2
    { oauth2ClientId          = clientId
    , oauth2ClientSecret      = Just clientSecret
    , oauth2AuthorizeEndpoint = "https://github.com/login/oauth/authorize"
                                  `withQuery` [scopeParam "," scopes]
    , oauth2TokenEndpoint     = "https://github.com/login/oauth/access_token"
    , oauth2RedirectUri       = Nothing
    }
