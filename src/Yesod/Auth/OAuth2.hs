{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
--
-- Generic OAuth2 plugin for Yesod
--
-- See "Yesod.Auth.OAuth2.GitHub" for example usage.
--
module Yesod.Auth.OAuth2
    ( OAuth2(..)
    , FetchCreds
    , Manager
    , OAuth2Token(..)
    , Creds(..)
    , oauth2Url
    , authOAuth2
    , authOAuth2Widget

    -- * Reading our @'credsExtra'@ keys
    , getAccessToken
    , getUserResponseJSON
    , getUserResponse
    ) where

import Data.Aeson (FromJSON, eitherDecode)
import Data.ByteString.Lazy (ByteString, fromStrict)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2
import Safe (fromJustNote)
import Yesod.Auth
import Yesod.Auth.OAuth2.Dispatch
import Yesod.Core.Widget

oauth2Url :: Text -> AuthRoute
oauth2Url name = PluginR name ["forward"]

-- | Create an @'AuthPlugin'@ for the given OAuth2 provider
--
-- Presents a generic @"Login via name"@ link
--
authOAuth2 :: YesodAuth m => Text -> OAuth2 -> FetchCreds m -> AuthPlugin m
authOAuth2 name = authOAuth2Widget [whamlet|Login via #{name}|] name

-- | Create an @'AuthPlugin'@ for the given OAuth2 provider
--
-- Allows passing a custom widget for the login link. See @'oauth2Eve'@ for an
-- example.
--
authOAuth2Widget
    :: YesodAuth m
    => WidgetT m IO ()
    -> Text
    -> OAuth2
    -> FetchCreds m
    -> AuthPlugin m
authOAuth2Widget widget name oauth getCreds =
    AuthPlugin name (dispatchAuthRequest name oauth getCreds) login
  where
    login tm = [whamlet|<a href=@{tm $ oauth2Url name}>^{widget}|]

-- | Read from the values set via @'setExtra'@
--
-- This is unsafe.
--
getAccessToken :: Creds m -> AccessToken
getAccessToken = AccessToken
    . fromJustNote "yesod-auth-oauth2 bug: credsExtra without accessToken"
    . lookup "accessToken" . credsExtra

-- | Read from the values set via @'setExtra'@
--
-- This is unsafe.
--
getUserResponseJSON :: Creds m -> ByteString
getUserResponseJSON = fromStrict . encodeUtf8
    . fromJustNote "yesod-auth-oauth2 bug: credsExtra without userResponseJSON"
    . lookup "userResponseJSON" . credsExtra

-- | Read from the values set via @'setExtra'@
--
-- This is unsafe if the key is missing, but safe with respect to parsing
-- errors.
--
getUserResponse :: FromJSON a => Creds m -> Either String a
getUserResponse = eitherDecode . getUserResponseJSON
