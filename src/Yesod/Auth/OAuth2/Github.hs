{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.Github
    ( oauth2Github
    , defaultScopes
    ) where

import Yesod.Auth.OAuth2.Provider
import Yesod.Auth.OAuth2.UserId

oauth2Github :: [Scope] -> Provider m UserId
oauth2Github scopes = Provider
    { pName = "github"
    , pAuthorizeEndpoint = const $ AuthorizeEndpoint
        $ "http://github.com/login/oauth/authorize" `withQuery`
            [ scopeParam "," scopes
            ]
    , pAccessTokenEndpoint = "http://github.com/login/oauth/access_token"
    , pFetchUserProfile = authGetProfile "https://api.github.com/user"
    }

defaultScopes :: [Scope]
defaultScopes = ["user:email"]
