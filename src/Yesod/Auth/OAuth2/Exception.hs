module Yesod.Auth.OAuth2.Exception
  ( YesodOAuth2Exception (..)
  ) where

import Control.Exception.Safe
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)

data YesodOAuth2Exception
  = -- | HTTP error during OAuth2 handshake
    --
    -- Plugin name and JSON-encoded @OAuth2Error@ from @hoauth2@.
    OAuth2Error Text ByteString
  | -- | User profile was not as expected
    --
    -- Plugin name and Aeson parse error message.
    JSONDecodingError Text String
  | -- | Other error conditions
    --
    -- Plugin name and error message.
    GenericError Text String
  deriving (Show)

instance Exception YesodOAuth2Exception
