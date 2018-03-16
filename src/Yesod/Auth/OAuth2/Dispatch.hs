{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TemplateHaskell #-}
module Yesod.Auth.OAuth2.Dispatch
    ( FetchCreds
    , dispatchAuthRequest
    ) where

import Control.Exception.Safe (throwString, tryIO)
import Control.Monad (unless, (<=<))
import Data.Monoid ((<>))
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2
import System.Random (newStdGen, randomRs)
import URI.ByteString.Extension
import Yesod.Auth
import Yesod.Auth.OAuth2.ErrorResponse (onErrorResponse)
import Yesod.Core

-- | How to take an @'OAuth2Token'@ and retrieve user credentials
type FetchCreds m = Manager -> OAuth2Token -> IO (Creds m)

-- | Dispatch the various OAuth2 handshake routes
dispatchAuthRequest
    :: Text             -- ^ Name
    -> OAuth2           -- ^ Service details
    -> FetchCreds m     -- ^ How to get credentials
    -> Text             -- ^ Method
    -> [Text]           -- ^ Path pieces
    -> AuthHandler m TypedContent
dispatchAuthRequest name oauth2 _ "GET" ["forward"] = dispatchForward name oauth2
dispatchAuthRequest name oauth2 getCreds "GET" ["callback"] = dispatchCallback name oauth2 getCreds
dispatchAuthRequest _ _ _ _ _ = notFound

-- | Handle @GET \/forward@
--
-- 1. Set a random CSRF token in our session
-- 2. Redirect to the Provider's authorization URL
--
dispatchForward :: Text -> OAuth2 -> AuthHandler m TypedContent
dispatchForward name oauth2 = do
    csrf <- setSessionCSRF $ tokenSessionKey name
    oauth2' <- withCallbackAndState name oauth2 csrf
    lift $ redirect $ toText $ authorizationUrl oauth2'

-- | Handle @GET \/callback@
--
-- 1. Verify the URL's CSRF token matches our session
-- 2. Use the code parameter to fetch an AccessToken for the Provider
-- 3. Use the AccessToken to construct a @'Creds'@ value for the Provider
--
dispatchCallback :: Text -> OAuth2 -> FetchCreds m -> AuthHandler m TypedContent
dispatchCallback name oauth2 getCreds = do
    csrf <- verifySessionCSRF $ tokenSessionKey name
    onErrorResponse errInvalidOAuth
    code <- requireGetParam "code"
    manager <- lift $ getsYesod authHttpManager
    oauth2' <- withCallbackAndState name oauth2 csrf
    token <- denyLeft $ fetchAccessToken manager oauth2' $ ExchangeToken code
    creds <- denyLeft $ tryIO $ getCreds manager token
    lift $ setCredsRedirect creds
  where
    -- On a Left result, log it and return an opaque permission-denied
    denyLeft :: (MonadHandler m, MonadLogger m, Show e) => IO (Either e a) -> m a
    denyLeft = either errInvalidOAuth pure <=< liftIO

    errInvalidOAuth :: (MonadHandler m, MonadLogger m, Show e) => e -> m a
    errInvalidOAuth err = do
        $(logError) $ T.pack $ "OAuth2 error: " <> show err
        permissionDenied "Invalid OAuth2 authentication attempt"

withCallbackAndState :: Text -> OAuth2 -> Text -> AuthHandler m OAuth2
withCallbackAndState name oauth2 csrf = do
    let url = PluginR name ["callback"]
    render <- getParentUrlRender
    let callbackText = render url

    callback <- maybe
        (throwString
            $ "Invalid callback URI: "
            <> T.unpack callbackText
            <> ". Not using an absolute Approot?"
        ) pure $ fromText callbackText

    pure oauth2
        { oauthCallback = Just callback
        , oauthOAuthorizeEndpoint = oauthOAuthorizeEndpoint oauth2
            `withQuery` [("state", encodeUtf8 csrf)]
        }

getParentUrlRender :: HandlerT child (HandlerT parent IO) (Route child -> Text)
getParentUrlRender = (.)
    <$> lift getUrlRender
    <*> getRouteToParent

-- | Set a random, 30-character value in the session
setSessionCSRF :: MonadHandler m => Text -> m Text
setSessionCSRF sessionKey = do
    csrfToken <- liftIO randomToken
    csrfToken <$ setSession sessionKey csrfToken
  where
    randomToken = T.pack . take 30 . randomRs ('a', 'z') <$> newStdGen

-- | Verify the callback provided the same CSRF token as in our session
verifySessionCSRF :: MonadHandler m => Text -> m Text
verifySessionCSRF sessionKey = do
    token <- requireGetParam "state"
    sessionToken <- lookupSession sessionKey
    deleteSession sessionKey

    unless (sessionToken == Just token)
        $ permissionDenied "Invalid OAuth2 state token"

    return token

requireGetParam :: MonadHandler m => Text -> m Text
requireGetParam key = do
    m <- lookupGetParam key
    maybe errInvalidArgs return m
  where
    errInvalidArgs = invalidArgs ["The '" <> key <> "' parameter is required"]

tokenSessionKey :: Text -> Text
tokenSessionKey name = "_yesod_oauth2_" <> name
