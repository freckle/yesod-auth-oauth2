{-# LANGUAGE DeriveDataTypeable #-}

module Yesod.Auth.OAuth2.Exception
    ( YesodOAuth2Exception(..)
    ) where

import Control.Exception.Safe
import qualified Data.ByteString.Lazy as BL
import Data.Text (Text)

-- | Provider name and error
--
-- The error is a lazy bytestring because it's most often encoded JSON.
--
-- Deprecated. Eventually, we'll return @Either@s all the way up.
--
data YesodOAuth2Exception = InvalidProfileResponse Text BL.ByteString
    deriving (Show, Typeable)
instance Exception YesodOAuth2Exception
