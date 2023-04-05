{-# LANGUAGE DeriveDataTypeable #-}

module Yesod.Auth.OAuth2.Exception
  ( YesodOAuth2Exception(..)
  ) where

import Control.Exception.Safe
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

data YesodOAuth2Exception
    = OAuth2Error Text ByteString
    -- ^ HTTP error during OAuth2 handshake
    --
    -- Plugin name and JSON-encoded @OAuth2Error@ from @hoauth2@.
    --
    | JSONDecodingError Text String
    -- ^ User profile was not as expected
    --
    -- Plugin name and Aeson parse error message.
    --
    | GenericError Text String
    -- ^ Other error conditions
    --
    -- Plugin name and error message.
    --
    deriving (Show, Typeable)

instance Exception YesodOAuth2Exception
