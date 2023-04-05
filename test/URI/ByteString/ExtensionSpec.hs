{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module URI.ByteString.ExtensionSpec
  ( spec
  ) where

import Test.Hspec

import Control.Exception (ErrorCall, evaluate)
import Data.List (isInfixOf)
import URI.ByteString
import URI.ByteString.Extension
import URI.ByteString.QQ

spec :: Spec
spec = do
  describe "IsString Scheme" $ it "works" $ do
    "https" `shouldBe` Scheme "https"

  describe "IsString Host" $ it "works" $ do
    "example.com" `shouldBe` Host "example.com"

  describe "IsString URIRef Relative" $ it "works" $ do
    "example.com/foo?bar=baz" `shouldBe` [relativeRef|example.com/foo?bar=baz|]

  describe "IsString URIRef Absolute" $ it "works" $ do
    "https://example.com/foo?bar=baz"
      `shouldBe` [uri|https://example.com/foo?bar=baz|]

  describe "fromText" $ do
    it "returns Just a URI for valid values, as the quasi-quoter would" $ do
      fromText "http://example.com/foo?bar=baz"
        `shouldBe` Just [uri|http://example.com/foo?bar=baz|]

    it "returns Nothing for invalid values" $ do
      fromText "Oh my, what did I do?" `shouldBe` Nothing

  describe "unsafeFromText" $ do
    it "returns a URI for valid values, as the quasi-quoter would" $ do
      unsafeFromText "http://example.com/foo?bar=baz"
        `shouldBe` [uri|http://example.com/foo?bar=baz|]

    it "raises for invalid values" $ do
      evaluate (unsafeFromText "Oh my, what did I do?")
        `shouldThrow` errorContaining "MissingColon"

  describe "toText" $ do
    it "serializes the URI to text" $ do
      toText [uri|https://example.com/foo?bar=baz|]
        `shouldBe` "https://example.com/foo?bar=baz"

  describe "fromRelative" $ do
    it "makes a URI absolute with a given host" $ do
      fromRelative "ftp" "foo.com" [relativeRef|/bar?baz=bat|]
        `shouldBe` [uri|ftp://foo.com/bar?baz=bat|]

  describe "withQuery" $ do
    it "appends a query to a URI" $ do
      let uriWithQuery = [uri|http://example.com|] `withQuery` [("foo", "bar")]

      uriWithQuery `shouldBe` [uri|http://example.com?foo=bar|]

    it "handles a URI with an existing query" $ do
      let
        uriWithQuery =
          [uri|http://example.com?foo=bar|] `withQuery` [("baz", "bat")]

      uriWithQuery `shouldBe` [uri|http://example.com?foo=bar&baz=bat|]

    -- This is arguably testing the internals of another package, but IMO
    -- it's worthwhile to show that you don't (and can't) pre-sanitize when
    -- using this function.
    it "handles santization of the query" $ do
      let
        uriWithQuery =
          [uri|http://example.com|] `withQuery` [("foo", "bar baz")]

      toText uriWithQuery `shouldBe` "http://example.com?foo=bar%20baz"

errorContaining :: String -> Selector ErrorCall
errorContaining msg = (msg `isInfixOf`) . show
