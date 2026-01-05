{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- OAuth2 plugin for http://github.com
--
-- * Authenticates against github
-- * Uses github user id as credentials identifier
module Yesod.Auth.OAuth2.GitHub
  ( oauth2GitHub
  , oauth2GitHubWidget
  , oauth2GitHubScoped
  , oauth2GitHubScopedWidget
  ) where

import qualified Data.Text as T
import Yesod.Auth.OAuth2.Prelude
import Yesod.Core (WidgetFor, whamlet)

newtype User = User Int

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "id"

pluginName :: Text
pluginName = "github"

defaultScopes :: [Text]
defaultScopes = ["user:email"]

oauth2GitHub :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2GitHub = oauth2GitHubScoped defaultScopes

oauth2GitHubWidget
  :: YesodAuth m => WidgetFor m () -> Text -> Text -> AuthPlugin m
oauth2GitHubWidget widget = oauth2GitHubScopedWidget widget defaultScopes

oauth2GitHubScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2GitHubScoped =
  oauth2GitHubScopedWidget [whamlet|Login via #{pluginName}|]

oauth2GitHubScopedWidget
  :: YesodAuth m => WidgetFor m () -> [Text] -> Text -> Text -> AuthPlugin m
oauth2GitHubScopedWidget widget scopes clientId clientSecret =
  authOAuth2Widget widget pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <-
      authGetProfile
        pluginName
        manager
        token
        "https://api.github.com/user"

    pure
      Creds
        { credsPlugin = pluginName
        , credsIdent = T.pack $ show userId
        , credsExtra = setExtra token userResponse
        }
 where
  oauth2 =
    OAuth2
      { oauth2ClientId = clientId
      , oauth2ClientSecret = clientSecret
      , oauth2AuthorizeEndpoint =
          "https://github.com/login/oauth/authorize"
            `withQuery` [scopeParam "," scopes]
      , oauth2TokenEndpoint = "https://github.com/login/oauth/access_token"
      , oauth2RedirectUri = Nothing
      }
