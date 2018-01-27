{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://www.google.com
--
-- * Authenticates against Google
-- * Uses Google user id as credentials identifier
--
-- If you were previously relying on the ability to parse email as the creds
-- identifier, you can still do that by overriding it in the creds returned by
-- the plugin. For example:
--
-- > --
-- > -- NOTE: proper use of Maybe/Either omitted for clarity.
-- > --
-- >
-- > parseEmail :: ByteString -> Text
-- > parseEmail = undefined
-- >
-- > authenticate creds = do
-- >     let userResponseJSON = fromJust $ lookup "userResponseJSON" credsExtra creds
-- >         userEmail = parseEmail userResponseJSON
-- >         updatedCreds = creds { credsIdent = userEmail }
-- >
-- >     -- continue normally with updatedCreds
--
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
        (User userId, userResponseJSON) <-
            authGetProfile pluginName manager token "https://www.googleapis.com/oauth2/v3/userinfo"

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = userId
            , credsExtra = setExtra token userResponseJSON
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
