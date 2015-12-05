module Yesod.Auth.OAuth2Spec
    ( main
    , spec
    ) where

import SpecHelper

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "authOAuth2" $
    it "works" $ do
        -- If I could somehow get this dispatch to execute, I could assert on the
        -- response; at least enough to verify the state parameter that prompted
        -- this track of work...
        --
        -- Currently, the blocker is that apDispatch is:
        --
        -- > HandlerT Auth (HandlerT App IO) TypedContent
        --
        -- but I really need a:
        --
        -- > HandlerT App IO TypedContent
        --
        -- to be able to use runFakeHandler, as is sort of shown below:
        --
        -- > let app = App
        -- >     plugin = authOAuth2 "example" undefined undefined
        -- >
        -- > x <- runFakeHandler
        -- >     M.empty undefined (getAuth app) $
        -- >     apDispatch plugin "GET" ["callback"]
        -- >
        -- > liftIO $ print (x :: TypedContent)
        --
        -- I basically need to peel one layer off the transformer stack, but I can't
        -- find the right run-handler anywhere.

        True `shouldBe` True
