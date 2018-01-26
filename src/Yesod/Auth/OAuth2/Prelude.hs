{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- |
--
-- Modules and support functions required by most or all provider
-- implementations. May also be useful for writing local providers.
--
module Yesod.Auth.OAuth2.Prelude
    ( YesodOAuth2Exception(..)
    , invalidProfileResponse

    -- * Helpers
    , fromProfileURL
    , scopeParam
    , maybeExtra

    -- * Text
    , Text
    , decodeUtf8
    , encodeUtf8

    -- * JSON
    , (.:)
    , (.:?)
    , (.=)
    , (<>)
    , FromJSON(..)
    , ToJSON(..)
    , eitherDecode
    , withObject

    -- * Exceptions
    , throwIO
    , tryIO

    -- * OAuth2
    , OAuth2(..)
    , OAuth2Token(..)
    , AccessToken(..)
    , RefreshToken(..)

    -- * HTTP
    , Manager
    , authGetJSON

    -- * Yesod
    , YesodAuth(..)
    , AuthPlugin(..)
    , Creds(..)

    -- * Bytestring URI types
    , Host(..)

    -- * Bytestring URI extensions
    , module URI.ByteString.Extension

    -- * Temporary, until I finish re-structuring modules
    , authOAuth2
    , authOAuth2Widget
    ) where

import Control.Exception.Safe
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BSL
import Data.Semigroup ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Conduit
import Network.OAuth.OAuth2
import URI.ByteString
import URI.ByteString.Extension
import Yesod.Auth
import Yesod.Auth.OAuth2

-- | Provider name and error
--
-- The error is a lazy bytestring because it's most often encoded JSON.
--
data YesodOAuth2Exception = InvalidProfileResponse Text BSL.ByteString
    deriving (Show, Typeable)
instance Exception YesodOAuth2Exception

-- | Construct an @'InvalidProfileResponse'@ exception from an @'OAuth2Error'@
--
-- This forces the @e@ in @'OAuth2Error' e@ to parse as a JSON @'Value'@ which
-- is then re-encoded for the exception message.
--
invalidProfileResponse :: Text -> OAuth2Error Value -> YesodOAuth2Exception
invalidProfileResponse name = InvalidProfileResponse name . encode

-- | Handle the common case of fetching Profile information from a JSON endpoint
--
-- Throws @'InvalidProfileResponse'@ if JSON parsing fails
--
fromProfileURL :: FromJSON a => Text -> URI -> (a -> Creds m) -> FetchCreds m
fromProfileURL name url toCreds manager token = do
    result <- authGetJSON manager (accessToken token) url
    either (throwIO . invalidProfileResponse name) (return . toCreds) result

-- | A tuple of @scope@ and the given scopes separated by a delimiter
scopeParam :: Text -> [Text] -> (ByteString, ByteString)
scopeParam d = ("scope",) . encodeUtf8 . T.intercalate d

-- | A helper for providing an optional value to credsExtra
maybeExtra :: Text -> Maybe Text -> [(Text, Text)]
maybeExtra k (Just v) = [(k, v)]
maybeExtra _ Nothing  = []
