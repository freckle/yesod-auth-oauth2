{-# LANGUAGE FlexibleInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module URI.ByteString.Extension where

import Data.ByteString (ByteString)
import Data.String (IsString(..))
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Lens.Micro

import qualified Data.ByteString.Char8 as C8

import URI.ByteString

instance IsString Scheme where
    fromString = Scheme . fromString

instance IsString Host where
    fromString = Host . fromString

instance IsString (URIRef Absolute) where
    fromString = either (error . show) id
        . parseURI strictURIParserOptions
        . C8.pack

instance IsString (URIRef Relative) where
    fromString = either (error . show) id
        . parseRelativeRef strictURIParserOptions
        . C8.pack

fromText :: Text -> Maybe URI
fromText = either (const Nothing) Just
    . parseURI strictURIParserOptions
    . encodeUtf8

unsafeFromText :: Text -> URI
unsafeFromText = either (error . show) id
    . parseURI strictURIParserOptions
    . encodeUtf8

toText :: URI -> Text
toText = decodeUtf8 . serializeURIRef'

fromRelative :: Scheme -> Host -> RelativeRef -> URI
fromRelative s h = flip withHost h . toAbsolute s

withHost :: URIRef a -> Host -> URIRef a
withHost u h = u & authorityL %~ maybe
    (Just $ Authority Nothing h Nothing)
    (\a -> Just $ a & authorityHostL .~ h)

withPath :: URIRef a -> ByteString -> URIRef a
withPath u p = u & pathL .~ p

withQuery :: URIRef a -> [(ByteString, ByteString)] -> URIRef a
withQuery u q = u & (queryL . queryPairsL) %~ (++ q)
