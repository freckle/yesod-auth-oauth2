{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://spotify.com
--
module Yesod.Auth.OAuth2.Spotify
    ( oauth2Spotify
    )
where

import Yesod.Auth.OAuth2.Prelude

newtype User = User Text

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User <$> o .: "id"

pluginName :: Text
pluginName = "spotify"

oauth2Spotify :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2Spotify scopes clientId clientSecret =
    authOAuth2 pluginName oauth2 $ \manager token -> do
        (User userId, userResponse) <- authGetProfile
            pluginName
            manager
            token
            "https://api.spotify.com/v1/me"

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = userId
            , credsExtra = setExtra token userResponse
            }
  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = Just clientSecret
        , oauthOAuthorizeEndpoint =
            "https://accounts.spotify.com/authorize"
                `withQuery` [scopeParam " " scopes]
        , oauthAccessTokenEndpoint = "https://accounts.spotify.com/api/token"
        , oauthCallback = Nothing
        }
