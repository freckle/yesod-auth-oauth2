{-# LANGUAGE DeriveDataTypeable, OverloadedStrings, QuasiQuotes #-}
module Yesod.Auth.OAuth2
    ( authOAuth2
    , oauth2Google
    , oauth2Learn
    ) where

import Control.Monad.IO.Class
import Data.ByteString          (ByteString)
import Data.Text                (Text)
import Data.Text.Encoding       (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Yesod.Auth
import Yesod.Form
import Yesod.Core
import Yesod.Auth.OAuth2.Internal

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
        dispatch "GET" ["forward"] = do
            tm <- getRouteToParent
            lift $ do
                render <- getUrlRender
                let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
                redirect $ bsToText $ authorizationUrl oauth'
        dispatch "GET" ["callback"] = do
            tm <- getRouteToParent
            render <- lift $ getUrlRender
            code <- lift $ runInputGet $ ireq textField "code"
            let oauth' = oauth { oauthCallback = Just $ encodeUtf8 $ render $ tm url }
            mtoken <- liftIO $ postAccessToken oauth' (encodeUtf8 code) (Just "authorization_code")
            case mtoken of
                Nothing -> permissionDenied "Couldn't get token"
                Just token -> do
                    creds <- liftIO $ mkCreds token
                    lift $ setCreds True creds
        dispatch _ _ = notFound
        login tm = do
            render <- getUrlRender
            let oaUrl = render $ tm $ oauth2Url name
            [whamlet| <a href=#{oaUrl}>Login via #{name} |]

oauth2Google :: Text -> Text -> OAuth2
oauth2Google clientId clientSecret = newOAuth2 { oauthClientId = encodeUtf8 clientId
                                               , oauthClientSecret = encodeUtf8 clientSecret
                                               , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth"
                                               , oauthAccessTokenEndpoint = "https://accounts.google.com/o/oauth2/token" }

oauth2Learn :: Text -> Text -> OAuth2
oauth2Learn clientId clientSecret = newOAuth2
    { oauthClientId            = encodeUtf8 clientId
    , oauthClientSecret        = encodeUtf8 clientSecret
    , oauthOAuthorizeEndpoint  = "http://learn.thoughtbot.com/oauth/authorize"
    , oauthAccessTokenEndpoint = "http://learn.thoughtbot.com/oauth/token"
    }

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
