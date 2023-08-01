{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

-- |
--
-- OAuth2 plugin for http://www.google.com
--
-- * Authenticates against Google
-- * Uses Google user id as credentials identifier
--
-- If you were previously relying on email as the creds identifier, you can
-- still do that (and more) by overriding it in the creds returned by the plugin
-- with any value read out of the new @userResponse@ key in @'credsExtra'@.
--
-- For example:
--
-- > data User = User { userEmail :: Text }
-- >
-- > instance FromJSON User where -- you know...
-- >
-- > authenticate creds = do
-- >     -- 'getUserResponseJSON' provided by "Yesod.Auth.OAuth" module
-- >     let Right email = userEmail <$> getUserResponseJSON creds
-- >         updatedCreds = creds { credsIdent = email }
-- >
-- >     -- continue normally with updatedCreds
module Yesod.Auth.OAuth2.Google
  ( oauth2Google
  , oauth2GoogleWidget
  , oauth2GoogleScoped
  , oauth2GoogleScopedWidget
  ) where

import Yesod.Auth.OAuth2.Prelude
import Yesod.Core (WidgetFor, whamlet)

newtype User = User Text

instance FromJSON User where
  parseJSON =
    withObject "User" $ \o ->
      User
        -- Required for data backwards-compatibility
        <$> (("google-uid:" <>) <$> o .: "sub")

pluginName :: Text
pluginName = "google"

defaultScopes :: [Text]
defaultScopes = ["openid", "email"]

oauth2Google :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Google = oauth2GoogleScoped defaultScopes

oauth2GoogleWidget
  :: YesodAuth m => WidgetFor m () -> Text -> Text -> AuthPlugin m
oauth2GoogleWidget widget = oauth2GoogleScopedWidget widget defaultScopes

oauth2GoogleScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2GoogleScoped =
  oauth2GoogleScopedWidget [whamlet|Login via #{pluginName}|]

oauth2GoogleScopedWidget
  :: YesodAuth m => WidgetFor m () -> [Text] -> Text -> Text -> AuthPlugin m
oauth2GoogleScopedWidget widget scopes clientId clientSecret =
  authOAuth2Widget widget pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <-
      authGetProfile
        pluginName
        manager
        token
        "https://www.googleapis.com/oauth2/v3/userinfo"

    pure
      Creds
        { credsPlugin = pluginName
        , credsIdent = userId
        , credsExtra = setExtra token userResponse
        }
 where
  oauth2 =
    OAuth2
      { oauth2ClientId = clientId
      , oauth2ClientSecret = Just clientSecret
      , oauth2AuthorizeEndpoint =
          "https://accounts.google.com/o/oauth2/auth"
            `withQuery` [scopeParam " " scopes]
      , oauth2TokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
      , oauth2RedirectUri = Nothing
      }
