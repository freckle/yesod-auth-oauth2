{-# LANGUAGE TypeApplications #-}

module Yesod.Auth.OAuth2.Random
    ( randomText
    ) where

import Crypto.Random (MonadRandom, getRandomBytes)
import Data.ByteArray.Encoding (Base(Base64), convertToBase)
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8)

randomText
    :: MonadRandom m
    => Int
    -- ^ Size in Bytes (note necessarily characters)
    -> m Text
randomText size =
    decodeUtf8 . convertToBase @ByteString Base64 <$> getRandomBytes size
