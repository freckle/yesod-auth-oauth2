{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://twitch.tv
--
-- * Authenticates against twitch
-- * Uses twitch user id as credentials identifier
--
module Yesod.Auth.OAuth2.Twitch
  ( oauth2Twitch
  , oauth2TwitchScoped
  ) where

import           Yesod.Auth.OAuth2.Prelude

import qualified Data.Text.Encoding            as T

newtype User = User Text

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "user_id"

pluginName :: Text
pluginName = "twitch"

defaultScopes :: [Text]
defaultScopes = ["user:read:email"]

oauth2Twitch :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Twitch = oauth2TwitchScoped defaultScopes

oauth2TwitchScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2TwitchScoped scopes clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <- authGetProfile
      pluginName
      manager
      token
      "https://id.twitch.tv/oauth2/validate"

    pure Creds { credsPlugin = pluginName
               , credsIdent  = userId
               , credsExtra  = setExtra token userResponse
               }
 where
  oauth2 = OAuth2
    { oauth2ClientId          = clientId
    , oauth2ClientSecret      = Just clientSecret
    , oauth2AuthorizeEndpoint = "https://id.twitch.tv/oauth2/authorize"
                                  `withQuery` [scopeParam " " scopes]
    , oauth2TokenEndpoint     = "https://id.twitch.tv/oauth2/token"
                                  `withQuery` [ ("client_id", T.encodeUtf8 clientId)
                                              , ( "client_secret"
                                                , T.encodeUtf8 clientSecret
                                                )
                                              ]
    , oauth2RedirectUri       = Nothing
    }
