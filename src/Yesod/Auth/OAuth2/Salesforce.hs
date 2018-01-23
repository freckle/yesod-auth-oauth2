{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.Salesforce
    ( oauth2Salesforce
    , oauth2SalesforceSandbox
    , defaultScopes
    ) where

import Data.Aeson
import Data.Text (Text)
import Yesod.Auth.OAuth2.Provider

newtype UserId = UserId Text
    deriving ToIdent

instance FromJSON UserId where
    parseJSON = withObject "User" $ \o -> UserId <$> o .: "user_id"

oauth2Salesforce :: [Scope] -> Provider m UserId
oauth2Salesforce scopes = Provider
    { pName = "salesforce"
    , pAuthorizeEndpoint = const $ AuthorizeEndpoint
        $ "https://login.salesforce.com/services/oauth2/authorize" `withQuery`
            [ scopeParam " " scopes
            ]
    , pAccessTokenEndpoint = "https://login.salesforce.com/services/oauth2/token"
    , pFetchUserProfile = authGetProfile "https://login.salesforce.com/services/oauth2/userinfo"
    }

oauth2SalesforceSandbox :: [Scope] -> Provider m UserId
oauth2SalesforceSandbox scopes = Provider
    { pName = "salesforce-sandbox"
    , pAuthorizeEndpoint = const $ AuthorizeEndpoint
        $ "https://test.salesforce.com/services/oauth2/authorize" `withQuery`
            [ scopeParam " " scopes
            ]
    , pAccessTokenEndpoint = "https://test.salesforce.com/services/oauth2/token"
    , pFetchUserProfile = authGetProfile "https://test.salesforce.com/services/oauth2/userinfo"
    }

defaultScopes :: [Scope]
defaultScopes = ["openid", "email", "api"]
