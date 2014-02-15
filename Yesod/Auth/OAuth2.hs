{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
-- |
--
-- Generic OAuth2 plugin for Yesod
--
-- * See Yesod.Auth.OAuth2.Learn for example usage.
--
module Yesod.Auth.OAuth2
    ( authOAuth2
    , oauth2Url
    , module Network.OAuth.OAuth2
    ) where

import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Network.OAuth.OAuth2
import Yesod.Auth
import Yesod.Core
import Yesod.Form

oauth2Url :: Text -> AuthRoute
oauth2Url name = PluginR name ["forward"]

authOAuth2 :: YesodAuth m
           => Text   -- ^ Service name
           -> OAuth2 -- ^ Service details

           -- | This function defines how to take an @'AccessToken'@ and
           --   retrive additional information about the user, to be set
           --   in the session as @'Creds'@. Usually this means a second
           --   authorized request to @api/me.json@.
           -> (AccessToken -> IO (Creds m))
           -> AuthPlugin m
authOAuth2 name oauth getCreds = AuthPlugin name dispatch login

    where
        url = PluginR name ["callback"]

        withCallback = do
            tm <- getRouteToParent
            render <- lift $ getUrlRender
            return $ oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }

        dispatch "GET" ["forward"] = do
            authUrl <- fmap (bsToText . authorizationUrl) withCallback
            lift $ redirect authUrl

        dispatch "GET" ["callback"] = do
            code <- lift $ runInputGet $ ireq textField "code"
            oauth' <- withCallback
            result <- liftIO $ fetchAccessToken oauth' (encodeUtf8 code)
            case result of
                Left _ -> permissionDenied "Unable to retreive OAuth2 token"
                Right token -> do
                    creds <- liftIO $ getCreds token
                    lift $ setCreds True creds

        dispatch _ _ = notFound

        login tm = do
            render <- getUrlRender
            let oaUrl = render $ tm $ oauth2Url name
            [whamlet| <a href=#{oaUrl}>Login via #{name} |]

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
