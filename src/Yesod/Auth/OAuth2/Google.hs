{-# LANGUAGE OverloadedStrings #-}
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
--
module Yesod.Auth.OAuth2.Google
    ( oauth2Google
    , oauth2GoogleScoped
    ) where

import Yesod.Auth.OAuth2.Prelude

newtype User = User Text

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User
        -- Required for data backwards-compatibility
        <$> (("google-uid:" <>) <$> o .: "sub")

pluginName :: Text
pluginName = "google"

defaultScopes :: [Text]
defaultScopes = ["openid", "email"]

oauth2Google :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Google = oauth2GoogleScoped defaultScopes

oauth2GoogleScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2GoogleScoped scopes clientId clientSecret =
    authOAuth2 pluginName oauth2 $ \manager token -> do
        (User userId, userResponse) <-
            authGetProfile pluginName manager token "https://www.googleapis.com/oauth2/v3/userinfo"

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = userId
            , credsExtra = setExtra token userResponse
            }
  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth" `withQuery`
            [ scopeParam "+" scopes
            ]
        , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
        , oauthCallback = Nothing
        }
