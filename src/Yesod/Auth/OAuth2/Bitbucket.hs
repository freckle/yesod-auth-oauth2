{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://bitbucket.com
--
-- * Authenticates against bitbucket
-- * Uses bitbucket uuid as credentials identifier
--
module Yesod.Auth.OAuth2.Bitbucket
    ( oauth2Bitbucket
    , oauth2BitbucketScoped
    ) where

import Yesod.Auth.OAuth2.Prelude

import qualified Data.Text as T

newtype User = User Text

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User
        <$> o .: "uuid"

pluginName :: Text
pluginName = "bitbucket"

defaultScopes :: [Text]
defaultScopes = ["account"]

oauth2Bitbucket :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Bitbucket = oauth2BitbucketScoped defaultScopes

oauth2BitbucketScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2BitbucketScoped scopes clientId clientSecret =
    authOAuth2 pluginName oauth2 $ \manager token -> do
        (User userId, userResponseJSON) <-
            authGetProfile pluginName manager token "https://api.bitbucket.com/2.0/user"

        pure Creds
            { credsPlugin = pluginName
            -- FIXME: Preserved bug. This should just be userId (it's already
            -- a Text), but because this code was shipped, folks likely have
            -- Idents in their database like @"\"...\""@, and if we fixed this
            -- they would need migrating. We're keeping it for now as it's a
            -- minor wart. Breaking typed APIs is one thing, causing data to go
            -- invalid is another.
            , credsIdent = T.pack $ show userId
            , credsExtra = setExtra token userResponseJSON
            }
  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://bitbucket.com/site/oauth2/authorize" `withQuery`
            [ scopeParam "," scopes
            ]
        , oauthAccessTokenEndpoint = "https://bitbucket.com/site/oauth2/access_token"
        , oauthCallback = Nothing
        }
