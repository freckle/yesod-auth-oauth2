{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
-- |
--
-- Modules and support functions required by most or all provider
-- implementations. May also be useful for writing local providers.
--
module Yesod.Auth.OAuth2.Prelude
  ( authGetProfile
  , scopeParam
  , setExtra

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
  , URI
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
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding
import Network.HTTP.Conduit
import Network.OAuth.OAuth2.Compat
import URI.ByteString
import URI.ByteString.Extension
import Yesod.Auth
import Yesod.Auth.OAuth2
import qualified Yesod.Auth.OAuth2.Exception as YesodOAuth2Exception

-- | Retrieve a user's profile as JSON
--
-- The response should be parsed only far enough to read the required
-- @'credsIdent'@. Additional information should either be re-parsed by or
-- fetched via additional requests by consumers.
--
authGetProfile
  :: FromJSON a
  => Text
  -> Manager
  -> OAuth2Token
  -> URI
  -> IO (a, BL.ByteString)
authGetProfile name manager token url = do
  resp <- fromAuthGet name =<< authGetBS manager (accessToken token) url
  decoded <- fromAuthJSON name resp
  pure (decoded, resp)

-- | Throws a @Left@ result as an @'YesodOAuth2Exception'@
fromAuthGet :: Text -> Either BL.ByteString BL.ByteString -> IO BL.ByteString
fromAuthGet _ (Right bs) = pure bs -- nice
fromAuthGet name (Left err) =
  throwIO $ YesodOAuth2Exception.OAuth2Error name err

-- | Throws a decoding error as an @'YesodOAuth2Exception'@
fromAuthJSON :: FromJSON a => Text -> BL.ByteString -> IO a
fromAuthJSON name =
  either (throwIO . YesodOAuth2Exception.JSONDecodingError name) pure
    . eitherDecode

-- | A tuple of @\"scope\"@ and the given scopes separated by a delimiter
scopeParam :: Text -> [Text] -> (ByteString, ByteString)
scopeParam d = ("scope", ) . encodeUtf8 . T.intercalate d

-- brittany-disable-next-binding

-- | Construct part of @'credsExtra'@
--
-- Always the following keys:
--
-- - @accessToken@: to support follow-up requests
-- - @userResponse@: to support getting additional information
--
-- May set the following keys:
--
-- - @refreshToken@: if the provider supports refreshing the @accessToken@
--
setExtra :: OAuth2Token -> BL.ByteString -> [(Text, Text)]
setExtra token userResponse =
    [ ("accessToken", atoken $ accessToken token)
    , ("userResponse", decodeUtf8 $ BL.toStrict userResponse)
    ]
    <> maybe [] (pure . ("refreshToken", ) . rtoken) (refreshToken token)
