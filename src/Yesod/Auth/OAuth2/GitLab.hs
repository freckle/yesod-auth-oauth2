{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.GitLab
    ( oauth2GitLab
    , oauth2GitLabHostScopes
    , defaultHost
    , defaultScopes
    )
where

import Yesod.Auth.OAuth2.Prelude

import qualified Data.Text as T

newtype User = User Int

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User <$> o .: "id"

pluginName :: Text
pluginName = "gitlab"

defaultHost :: URI
defaultHost = "https://gitlab.com"

defaultScopes :: [Text]
defaultScopes = ["read_user"]

-- | Authorize with @gitlab.com@ and @[\"read_user\"]@
--
-- To customize either of these values, use @'oauth2GitLabHostScopes'@ and pass
-- the default for the argument not being customized. Note that we require at
-- least @read_user@, so we can request the credentials identifier.
--
-- > oauth2GitLabHostScopes defaultHost ["api", "read_user"]
-- > oauth2GitLabHostScopes "https://gitlab.example.com" defaultScopes
--
oauth2GitLab :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2GitLab = oauth2GitLabHostScopes defaultHost defaultScopes

oauth2GitLabHostScopes
    :: YesodAuth m => URI -> [Text] -> Text -> Text -> AuthPlugin m
oauth2GitLabHostScopes host scopes clientId clientSecret =
    authOAuth2 pluginName oauth2 $ \manager token -> do
        (User userId, userResponse) <-
            authGetProfile pluginName manager token
            $ host
            `withPath` "/api/v4/user"

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = T.pack $ show userId
            , credsExtra = setExtra token userResponse
            }
  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint =
            host
            `withPath` "/oauth/authorize"
            `withQuery` [scopeParam " " scopes]
        , oauthAccessTokenEndpoint = host `withPath` "/oauth/token"
        , oauthCallback = Nothing
        }
