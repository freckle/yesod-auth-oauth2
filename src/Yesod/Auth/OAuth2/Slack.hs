{-# LANGUAGE OverloadedStrings #-}
-- |
-- OAuth2 plugin for https://slack.com/
--
-- * Authenticates against slack
-- * Uses slack user id as credentials identifier
--
module Yesod.Auth.OAuth2.Slack
    ( SlackScope(..)
    , oauth2Slack
    , oauth2SlackScoped
    ) where

import Yesod.Auth.OAuth2.Prelude

import Network.HTTP.Client
    (httpLbs, parseUrlThrow, responseBody, setQueryString)

data SlackScope
    = SlackBasicScope
    | SlackEmailScope
    | SlackTeamScope
    | SlackAvatarScope

scopeText :: SlackScope -> Text
scopeText SlackBasicScope = "identity.basic"
scopeText SlackEmailScope = "identity.email"
scopeText SlackTeamScope = "identity.team"
scopeText SlackAvatarScope = "identity.avatar"

newtype User = User Text

instance FromJSON User where
    parseJSON = withObject "User" $ \root -> do
        o <- root .: "user"
        User <$> o .: "id"

pluginName :: Text
pluginName = "slack"

defaultScopes :: [SlackScope]
defaultScopes = [SlackBasicScope]

oauth2Slack :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Slack = oauth2SlackScoped defaultScopes

oauth2SlackScoped :: YesodAuth m => [SlackScope] -> Text -> Text -> AuthPlugin m
oauth2SlackScoped scopes clientId clientSecret =
    authOAuth2 pluginName oauth2 $ \manager token -> do
        let param = encodeUtf8 $ atoken $ accessToken token
        req <- setQueryString [("token", Just param)]
            <$> parseUrlThrow "https://slack.com/api/users.identity"
        userResponseJSON <- responseBody <$> httpLbs req manager

        either
            (const $ throwIO $ InvalidProfileResponse pluginName userResponseJSON)
            (\(User userId) -> pure Creds
                { credsPlugin = pluginName
                , credsIdent = userId
                , credsExtra = setExtra token userResponseJSON
                }
            )
            $ eitherDecode userResponseJSON
  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://slack.com/oauth/authorize" `withQuery`
            [ scopeParam "," $ map scopeText scopes
            ]
        , oauthAccessTokenEndpoint = "https://slack.com/api/oauth.access"
        , oauthCallback = Nothing
        }
