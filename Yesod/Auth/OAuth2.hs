{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
--
-- Generic OAuth2 plugin for Yesod
--
-- * See Yesod.Auth.OAuth2.Learn for example usage.
--
module Yesod.Auth.OAuth2
    ( authOAuth2
    , oauth2Url
    , YesodOAuth2Exception(..)
    , module Network.OAuth.OAuth2
    ) where

import Control.Exception.Lifted
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Text (Text)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Typeable
import Network.OAuth.OAuth2
import Network.HTTP.Conduit(Manager)
import Yesod.Auth
import Yesod.Core
import Yesod.Form

import qualified Data.ByteString.Lazy as BSL

data YesodOAuth2Exception = InvalidProfileResponse
    Text           -- ^ Provider name
    BSL.ByteString -- ^ Aeson parse error
    deriving (Show, Typeable)

instance Exception YesodOAuth2Exception

oauth2Url :: Text -> AuthRoute
oauth2Url name = PluginR name ["forward"]

authOAuth2 :: YesodAuth m
           => Text   -- ^ Service name
           -> OAuth2 -- ^ Service details
           -> (Manager -> AccessToken -> IO (Creds m))
           -- ^ This function defines how to take an @'AccessToken'@ and
           --   retrieve additional information about the user, to be
           --   set in the session as @'Creds'@. Usually this means a
           --   second authorized request to @api/me.json@.
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
            master <- lift getYesod
            result <- liftIO $ fetchAccessToken (authHttpManager master) oauth' (encodeUtf8 code)
            case result of
                Left _ -> permissionDenied "Unable to retreive OAuth2 token"
                Right token -> do
                    creds <- liftIO $ getCreds (authHttpManager master) token
                    lift $ setCredsRedirect creds

        dispatch _ _ = notFound

        login tm = do
            render <- getUrlRender
            let oaUrl = render $ tm $ oauth2Url name
            [whamlet| <a href=#{oaUrl}>Login via #{name} |]

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
