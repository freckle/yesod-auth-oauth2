{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.UserId
    ( UserId(..)
    , UserIdText(..)
    ) where

import Data.Aeson
import Data.Text (Text)
import Yesod.Auth.OAuth2.Provider (ToIdent(..))

-- | Parse-able type to use for responses with an integer @id@ field
newtype UserId = UserId Int
    deriving ToIdent

instance FromJSON UserId where
    parseJSON = withObject "User" $ \o -> UserId <$> o .: "id"

-- | Parse-able type to use for responses with a textual @id@ field
newtype UserIdText = UserIdText Text
    deriving ToIdent

instance FromJSON UserIdText where
    parseJSON = withObject "User" $ \o -> UserIdText <$> o .: "id"
