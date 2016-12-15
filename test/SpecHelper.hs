{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module SpecHelper
    ( App(..)
    , Widget
    , resourcesApp
    , module Test.Hspec
    , module Yesod
    , module Yesod.Auth
    , module Yesod.Auth.OAuth2
    ) where

import Test.Hspec

import Data.Text (Text)

import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2

data App = App

mkYesod "App" [parseRoutes| / R GET |]

instance Yesod App
instance YesodAuth App where
    type AuthId App = Text

    authHttpManager = undefined
    authPlugins = undefined
    authenticate = undefined
    loginDest = undefined
    logoutDest = undefined
    maybeAuthId = undefined

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getR :: Handler ()
getR = return ()
