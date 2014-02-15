{-# LANGUAGE OverloadedStrings, QuasiQuotes #-}
module Yesod.Auth.OAuth2
    ( authOAuth2
    , oauth2Url
    , oauth2Google
    , oauth2Learn
    , module Network.OAuth.OAuth2
    ) where

import Control.Monad.IO.Class
import Data.ByteString          (ByteString)
import Data.Text                (Text)
import Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Yesod.Auth
import Yesod.Form
import Yesod.Core
import Network.OAuth.OAuth2

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
authOAuth2 name oauth mkCreds = AuthPlugin name dispatch login

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
                    creds <- liftIO $ mkCreds token
                    lift $ setCreds True creds

        dispatch _ _ = notFound

        login tm = do
            render <- getUrlRender
            let oaUrl = render $ tm $ oauth2Url name
            [whamlet| <a href=#{oaUrl}>Login via #{name} |]

oauth2Google :: Text -> Text -> OAuth2
oauth2Google clientId clientSecret = OAuth2
    { oauthClientId            = encodeUtf8 clientId
    , oauthClientSecret        = encodeUtf8 clientSecret
    , oauthOAuthorizeEndpoint  = "https://accounts.google.com/o/oauth2/auth"
    , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token"
    , oauthCallback            = Nothing
    }

oauth2Learn :: Text -> Text -> OAuth2
oauth2Learn clientId clientSecret = OAuth2
    { oauthClientId            = encodeUtf8 clientId
    , oauthClientSecret        = encodeUtf8 clientSecret
    , oauthOAuthorizeEndpoint  = "http://learn.thoughtbot.com/oauth/authorize"
    , oauthAccessTokenEndpoint = "http://learn.thoughtbot.com/oauth/token"
    , oauthCallback            = Nothing
    }

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
