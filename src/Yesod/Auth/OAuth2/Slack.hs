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
    )
where

import Yesod.Auth.OAuth2.Prelude

import Network.HTTP.Client
    (httpLbs, parseUrlThrow, responseBody, setQueryString)
import Yesod.Auth.OAuth2.Exception as YesodOAuth2Exception

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
        userResponse <- responseBody <$> httpLbs req manager

        either
                (throwIO . YesodOAuth2Exception.JSONDecodingError pluginName)
                (\(User userId) -> pure Creds
                    { credsPlugin = pluginName
                    , credsIdent = userId
                    , credsExtra = setExtra token userResponse
                    }
                )
            $ eitherDecode userResponse
  where
    oauth2 = OAuth2
        { oauth2ClientId = clientId
        , oauth2ClientSecret = Just clientSecret
        , oauth2AuthorizeEndpoint =
            "https://slack.com/oauth/authorize"
                `withQuery` [scopeParam "," $ map scopeText scopes]
        , oauth2TokenEndpoint = "https://slack.com/api/oauth.access"
        , oauth2RedirectUri = Nothing
        }
