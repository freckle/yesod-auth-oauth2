{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://google.com
--
-- * Note: this module is unfinished, do not use.
--
module Yesod.Auth.OAuth2.Google
    ( oauth2Google
    , module Yesod.Auth.OAuth2
    ) where

import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Yesod.Auth
import Yesod.Auth.OAuth2

oauth2Google :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Google clientId clientSecret = authOAuth2 "google"
    (OAuth2
        { oauthClientId            = encodeUtf8 clientId
        , oauthClientSecret        = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint  = "https://accounts.google.com/o/oauth2/auth"
        , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
        , oauthCallback            = Nothing
        })
    undefined -- TODO
