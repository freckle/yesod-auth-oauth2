{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.EveOnline
    ( oauth2EveOnline
    ) where

import Data.Aeson
import Yesod.Auth.OAuth2.Provider

newtype CharId = CharId Int
    deriving ToIdent

instance FromJSON CharId where
    parseJSON = withObject "Character" $ \o -> CharId <$> o .: "CharacterId"

oauth2EveOnline :: [Scope] -> Provider m CharId
oauth2EveOnline scopes = Provider
    { pName = "eveonline"
    , pAuthorizeEndpoint = const $ AuthorizeEndpoint
        $ "https://login.eveonline.com/oauth/authorize" `withQuery`
            [ ("response_type", "code")
            , scopeParam " " scopes
            ]
    , pAccessTokenEndpoint = "https://login.eveonline.com/oauth/token"
    , pFetchUserProfile = authGetProfile "https://login.eveonline.com/oauth/verify"
    }
