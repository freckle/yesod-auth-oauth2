-- |
--
-- This is a single-file example of using yesod-auth-oauth2.
--
-- It can be run with:
--
-- > stack build --flag yesod-auth-oauth2:example
-- > stack exec yesod-auth-oauth2-example
-- > $BROWSER http://localhost:3000
--
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ViewPatterns #-}

module Main where

import Data.Monoid ((<>))
import Data.Text (Text)
import LoadEnv
import Network.HTTP.Conduit
import Network.Wai.Handler.Warp (runEnv)
import System.Environment (getEnv)
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth2.Github

import qualified Data.Text as T

data OAuthKeys = OAuthKeys
    { oauthKeysClientId :: Text
    , oauthKeysClientSecret :: Text
    }

loadOAuthKeysEnv :: String -> IO OAuthKeys
loadOAuthKeysEnv prefix = OAuthKeys
    <$> (getEnvT $ prefix <> "_CLIENT_ID")
    <*> (getEnvT $ prefix <> "_CLIENT_SECRET")

  where
    getEnvT = fmap T.pack . getEnv

data App = App
    { appHttpManager :: Manager
    , appGithubKeys :: OAuthKeys
    -- , appGoogleKeys :: OAuthKeys
    -- , etc...
    }

mkYesod "App" [parseRoutes|
    / RootR GET
    /auth AuthR Auth getAuth
|]

instance Yesod App where
    -- redirect_uri must be absolute to avoid callback mismatch error
    approot = ApprootStatic "http://localhost:3000"

instance YesodAuth App where
    type AuthId App = Text
    loginDest _ = RootR
    logoutDest _ = RootR

    -- Disable any attempt to read persisted authenticated state
    maybeAuthId = return Nothing

    -- Copy the Creds response into the session for viewing after
    authenticate c = do
        mapM_ (uncurry setSession) $
            [ ("credsIdent", credsIdent c)
            , ("credsPlugin", credsPlugin c)
            ] ++ credsExtra c

        return $ Authenticated "1"

    authHttpManager = appHttpManager

    authPlugins m =
        [ oauth2Github
            (oauthKeysClientId $ appGithubKeys m)
            (oauthKeysClientSecret $ appGithubKeys m)
        -- , oauth2Google
        --     (oauthKeysClientId $ appGoogleKeys m)
        --     (oauthKeysClientSecret $ appGoogleKeys m)
        -- , etc...
        ]

instance RenderMessage App FormMessage where
    renderMessage _ _ = defaultFormMessage

getRootR :: Handler Html
getRootR = do
    sess <- getSession

    defaultLayout [whamlet|
        <h1>Yesod Auth OAuth2 Example
        <h2>
            <a href=@{AuthR LoginR}>Log in
        <h2>Session Information
        <pre style="word-wrap: break-word;">
            #{show sess}
    |]

mkFoundation :: IO App
mkFoundation = do
    loadEnv

    appHttpManager <- newManager tlsManagerSettings
    appGithubKeys <- loadOAuthKeysEnv "GITHUB"
    -- appGoogleKeys <- loadOAuthKeysEnv "GOOGLE"
    -- etc...

    return App{..}

main :: IO ()
main = runEnv 3000 =<< toWaiApp =<< mkFoundation
