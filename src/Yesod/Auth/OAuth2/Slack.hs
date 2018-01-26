{-# LANGUAGE OverloadedStrings #-}
-- |
-- OAuth2 plugin for https://slack.com/
--
-- * Authenticates against slack
-- * Uses slack user id as credentials identifier
-- * Returns name, access_token, email, avatar, team_id, and team_name as extras
--
module Yesod.Auth.OAuth2.Slack
    ( SlackScope(..)
    , oauth2Slack
    , oauth2SlackScoped
    ) where

import Yesod.Auth.OAuth2.Prelude

import Data.Maybe (catMaybes)
import qualified Network.HTTP.Conduit as HTTP

data SlackScope
    = SlackEmailScope
    | SlackTeamScope
    | SlackAvatarScope

data SlackUser = SlackUser
    { slackUserId :: Text
    , slackUserName :: Text
    , slackUserEmail :: Maybe Text
    , slackUserAvatarUrl :: Maybe Text
    , slackUserTeam :: Maybe SlackTeam
    }

data SlackTeam = SlackTeam
    { slackTeamId :: Text
    , slackTeamName :: Text
    }

instance FromJSON SlackUser where
    parseJSON = withObject "root" $ \root -> do
        user <- root .: "user"

        SlackUser
            <$> user .: "id"
            <*> user .: "name"
            <*> user .:? "email"
            <*> user .:? "image_512"
            <*> root .:? "team"

instance FromJSON SlackTeam where
    parseJSON = withObject "team" $ \team ->
        SlackTeam
            <$> team .: "id"
            <*> team .: "name"

-- | Auth with Slack
--
-- Requests @identity.basic@ scopes and uses the user's Slack ID as the @'Creds'@
-- identifier.
--
oauth2Slack :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Slack clientId clientSecret = oauth2SlackScoped clientId clientSecret []

-- | Auth with Slack
--
-- Requests custom scopes and uses the user's Slack ID as the @'Creds'@
-- identifier.
--
oauth2SlackScoped :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> [SlackScope]
             -> AuthPlugin m
oauth2SlackScoped clientId clientSecret scopes =
    authOAuth2 "slack" oauth fetchSlackProfile
  where
    oauth = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://slack.com/oauth/authorize" `withQuery`
            [ scopeParam "," $ "identity.basic" : map scopeText scopes
            ]
        , oauthAccessTokenEndpoint = "https://slack.com/api/oauth.access"
        , oauthCallback = Nothing
        }

scopeText :: SlackScope -> Text
scopeText SlackEmailScope = "identity.email"
scopeText SlackTeamScope = "identity.team"
scopeText SlackAvatarScope = "identity.avatar"

fetchSlackProfile :: Manager -> OAuth2Token -> IO (Creds m)
fetchSlackProfile manager token = do
    request
        <- HTTP.setQueryString [("token", Just $ encodeUtf8 $ atoken $ accessToken token)]
        <$> HTTP.parseUrlThrow "https://slack.com/api/users.identity"
    body <- HTTP.responseBody <$> HTTP.httpLbs request manager
    case eitherDecode body of
        Left _ -> throwIO $ InvalidProfileResponse "slack" body
        Right u -> return $ toCreds u token

toCreds :: SlackUser -> OAuth2Token -> Creds m
toCreds user token = Creds
    { credsPlugin = "slack"
    , credsIdent = slackUserId user
    , credsExtra = catMaybes
        [ Just ("name", slackUserName user)
        , Just ("access_token", atoken $ accessToken token)
        , (,) <$> pure "email" <*> slackUserEmail user
        , (,) <$> pure "avatar" <*> slackUserAvatarUrl user
        , (,) <$> pure "team_name" <*> (slackTeamName <$> slackUserTeam user)
        , (,) <$> pure "team_id" <*> (slackTeamId <$> slackUserTeam user)
        ]
    }
