{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.Google
    ( oauth2Google
    , defaultScopes
    ) where

import Data.Aeson
import Data.Text (Text)
import Yesod.Auth.OAuth2.Provider

newtype UserId = UserId Text
    deriving ToIdent

instance FromJSON UserId where
    parseJSON = withObject "User" $ \o -> UserId <$> o .: "sub"

oauth2Google :: [Scope] -> Provider m UserId
oauth2Google scopes = Provider
    { pName = "google"
    , pAuthorizeEndpoint = const $ AuthorizeEndpoint
        $ "https://accounts.google.com/o/oauth2/auth" `withQuery`
            [ scopeParam "+" scopes
            ]
    , pAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
    , pFetchUserProfile = authGetProfile "https://www.googleapis.com/oauth2/v3/userinfo"
    }

defaultScopes :: [Scope]
defaultScopes = ["openid", "email"]
