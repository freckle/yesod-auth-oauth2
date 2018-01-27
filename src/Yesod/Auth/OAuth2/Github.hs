{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://github.com
--
-- * Authenticates against github
-- * Uses github user id as credentials identifier
--
module Yesod.Auth.OAuth2.Github
    ( oauth2Github
    , oauth2GithubScoped
    ) where

import Yesod.Auth.OAuth2.Prelude

import qualified Data.Text as T

newtype User = User Int

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User
        <$> o .: "id"

pluginName :: Text
pluginName = "github"

defaultScopes :: [Text]
defaultScopes = ["user:email"]

oauth2Github :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Github = oauth2GithubScoped defaultScopes

oauth2GithubScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2GithubScoped scopes clientId clientSecret =
    authOAuth2 pluginName oauth2 $ \manager token -> do
        (User userId, userResponseJSON) <-
            authGetProfile pluginName manager token "https://api.github.com/user"

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = T.pack $ show userId
            , credsExtra = setExtra token userResponseJSON
            }
  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize" `withQuery`
            [ scopeParam "," scopes
            ]
        , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
        , oauthCallback = Nothing
        }
