module Yesod.Auth.OAuth2Spec
    ( main
    , spec
    ) where

import Test.Hspec
import Yesod.Auth.OAuth2

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "authOAuth2" $
    it "works" $
        True `shouldBe` True
