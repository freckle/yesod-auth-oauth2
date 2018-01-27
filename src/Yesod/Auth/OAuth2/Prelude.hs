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
    , authGetProfile
    , setExtra
    , scopeParam

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

    -- * Deprecated, until everything's moved over to @'authGetProfile'@
    , authGetJSON
    , fromProfileURL
    , maybeExtra
    ) where

import Control.Exception.Safe
import Data.Aeson
import Data.ByteString (ByteString)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
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
-- Deprecated. Eventually, we'll return @Either@s all the way up.
--
data YesodOAuth2Exception = InvalidProfileResponse Text BL.ByteString
    deriving (Show, Typeable)
instance Exception YesodOAuth2Exception

-- | Retrieve a user's profile as JSON
--
-- The response should be parsed only far enough to read the required
-- @'credsIdent'@. The raw response is returned as well, to be set in
-- @'credsExtra'@for consumers to re-use if information they seek is in the
-- response already.
--
-- Information requiring other requests should use the access token (also in
-- @'credsExtra'@ to make subsequent requests themselves.
--
authGetProfile
    :: FromJSON a
    => Text -- ^ Plugin name
    -> Manager
    -> OAuth2Token
    -> URI
    -> IO (a, BL.ByteString)
authGetProfile name manager token url = do
    resp <- fromAuthGet name =<< authGetBS manager (accessToken token) url
    decoded <- fromAuthJSON name resp
    pure (decoded, resp)

-- | Throws a @Left@ result as an @'InvalidProfileResponse'@
fromAuthGet :: Text -> Either (OAuth2Error Value) BL.ByteString -> IO BL.ByteString
fromAuthGet _ (Right bs) = pure bs -- nice
fromAuthGet name (Left err) = throwIO $ invalidProfileResponse name err

-- | Throws a decoding error as an @'InvalidProfileResponse'@
fromAuthJSON :: FromJSON a => Text -> BL.ByteString -> IO a
fromAuthJSON name =
    -- FIXME: unique exception constructors
    either (throwIO . InvalidProfileResponse name . BL8.pack) pure . eitherDecode

-- | Construct (part of) @'credsExtra'@ container the token and user response
setExtra :: OAuth2Token -> BL.ByteString -> [(Text, Text)]
setExtra token userResponseJSON =
    [ ("accessToken", atoken $ accessToken token)
    , ("userResponseJSON", decodeUtf8 $ BL.toStrict userResponseJSON)
    ]

-- | Construct an @'InvalidProfileResponse'@ exception from an @'OAuth2Error'@
--
-- This forces the @e@ in @'OAuth2Error' e@ to parse as a JSON @'Value'@ which
-- is then re-encoded for the exception message.
--
-- Deprecated.
--
invalidProfileResponse :: Text -> OAuth2Error Value -> YesodOAuth2Exception
invalidProfileResponse name = InvalidProfileResponse name . encode

-- | Handle the common case of fetching Profile information from a JSON endpoint
--
-- Throws @'InvalidProfileResponse'@ if JSON parsing fails
--
-- Deprecated.
--
fromProfileURL :: FromJSON a => Text -> URI -> (a -> Creds m) -> FetchCreds m
fromProfileURL name url toCreds manager token =
    toCreds . fst <$> authGetProfile name manager token url

-- | A tuple of @scope@ and the given scopes separated by a delimiter
scopeParam :: Text -> [Text] -> (ByteString, ByteString)
scopeParam d = ("scope",) . encodeUtf8 . T.intercalate d

-- | A helper for providing an optional value to credsExtra
maybeExtra :: Text -> Maybe Text -> [(Text, Text)]
maybeExtra k (Just v) = [(k, v)]
maybeExtra _ Nothing  = []
