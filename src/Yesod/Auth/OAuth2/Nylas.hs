{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.Nylas
    ( oauth2Nylas
    ) where

import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Client
    (applyBasicAuth, httpLbs, parseUrlThrow, responseBody)
import Network.OAuth.OAuth2 (AccessToken(..))
import URI.ByteString.Extension (withQuery)
import Yesod.Auth.OAuth2.Provider
import Yesod.Auth.OAuth2.UserId

oauth2Nylas :: Provider m UserIdText
oauth2Nylas = Provider
    { pName = "nylas"
    , pAuthorizeEndpoint = \cid -> AuthorizeEndpoint $
        "https://api.nylas.com/oauth/authorize" `withQuery`
            [ ("client_id", encodeUtf8 $ clientId cid)
            , ("response_type", "code")
            , ("scope", "email")
            ]
    , pAccessTokenEndpoint = "https://api.nylas.com/oauth/token"
    , pFetchUserProfile = \manager token -> do
        req <- applyBasicAuth (encodeUtf8 $ atoken token) ""
            <$> parseUrlThrow "https://api.nylas.com/account"
        Right . responseBody <$> httpLbs req manager
    }
