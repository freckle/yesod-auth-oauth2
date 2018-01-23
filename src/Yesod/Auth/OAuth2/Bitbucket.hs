{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.Bitbucket
    ( oauth2Bitbucket
    ) where

import Data.Aeson
import Data.Text (Text)
import Yesod.Auth.OAuth2.Provider

newtype UserId = UserId Text
    deriving ToIdent

instance FromJSON UserId where
    parseJSON = withObject "User" $ \o -> UserId <$> o .: "uuid"

oauth2Bitbucket :: [Scope] -> Provider m UserId
oauth2Bitbucket scopes = Provider
    { pName = "bitbucket"
    , pAuthorizeEndpoint = const $ AuthorizeEndpoint
        $ "https://bitbucket.com/site/oauth2/authorize" `withQuery`
            [ scopeParam "," scopes
            ]
    , pAccessTokenEndpoint = "https://bitbucket.com/site/oauth2/access_token"
    , pFetchUserProfile = authGetProfile "https://api.bitbucket.com/2.0/user"
    }
