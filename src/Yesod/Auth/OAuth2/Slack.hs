{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.Slack
    ( oauth2Slack
    , defaultScopes
    ) where

import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
    (httpLbs, parseUrlThrow, responseBody, setQueryString)
import Network.OAuth.OAuth2 (AccessToken(..))
import Yesod.Auth.OAuth2.Provider
import Yesod.Auth.OAuth2.UserId

oauth2Slack :: [Scope] -> Provider m UserIdText
oauth2Slack scopes = Provider
    { pName = "slack"
    , pAuthorizeEndpoint = const $ AuthorizeEndpoint
        $ "https://slack.com/oauth/authorize" `withQuery`
            [ scopeParam "," scopes
            ]
    , pAccessTokenEndpoint = "https://slack.com/api/oauth.access"
    , pFetchUserProfile = \manager token -> do
        request <- setQueryString [("token", Just $ encodeUtf8 $ atoken token)]
            <$> parseUrlThrow "https://slack.com/api/users.identity"
        Right . responseBody <$> httpLbs request manager
    }

defaultScopes :: [Scope]
defaultScopes = ["identity.basic"]
