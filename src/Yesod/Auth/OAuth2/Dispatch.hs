{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Yesod.Auth.OAuth2.Dispatch
    ( FetchToken
    , fetchAccessToken
    , fetchAccessToken2
    , FetchCreds
    , dispatchAuthRequest
    )
where

import Control.Exception.Safe
import Control.Monad (unless, (<=<))
import Crypto.Random (getRandomBytes)
import Data.ByteArray.Encoding (Base(Base64), convertToBase)
import Data.ByteString (ByteString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.TokenRequest (Errors)
import URI.ByteString.Extension
import Yesod.Auth hiding (ServerError)
import Yesod.Auth.OAuth2.ErrorResponse
import Yesod.Auth.OAuth2.Exception
import Yesod.Core hiding (ErrorResponse)

-- | How to fetch an @'OAuth2Token'@
--
-- This will be 'fetchAccessToken' or 'fetchAccessToken2'
--
type FetchToken
    = Manager -> OAuth2 -> ExchangeToken -> IO (OAuth2Result Errors OAuth2Token)

-- | How to take an @'OAuth2Token'@ and retrieve user credentials
type FetchCreds m = Manager -> OAuth2Token -> IO (Creds m)

-- | Dispatch the various OAuth2 handshake routes
dispatchAuthRequest
    :: Text             -- ^ Name
    -> OAuth2           -- ^ Service details
    -> FetchToken       -- ^ How to get a token
    -> FetchCreds m     -- ^ How to get credentials
    -> Text             -- ^ Method
    -> [Text]           -- ^ Path pieces
    -> AuthHandler m TypedContent
dispatchAuthRequest name oauth2 _ _ "GET" ["forward"] =
    dispatchForward name oauth2
dispatchAuthRequest name oauth2 getToken getCreds "GET" ["callback"] =
    dispatchCallback name oauth2 getToken getCreds
dispatchAuthRequest _ _ _ _ _ _ = notFound

-- | Handle @GET \/forward@
--
-- 1. Set a random CSRF token in our session
-- 2. Redirect to the Provider's authorization URL
--
dispatchForward :: Text -> OAuth2 -> AuthHandler m TypedContent
dispatchForward name oauth2 = do
    csrf <- setSessionCSRF $ tokenSessionKey name
    oauth2' <- withCallbackAndState name oauth2 csrf
    redirect $ toText $ authorizationUrl oauth2'

-- | Handle @GET \/callback@
--
-- 1. Verify the URL's CSRF token matches our session
-- 2. Use the code parameter to fetch an AccessToken for the Provider
-- 3. Use the AccessToken to construct a @'Creds'@ value for the Provider
--
dispatchCallback
    :: Text
    -> OAuth2
    -> FetchToken
    -> FetchCreds m
    -> AuthHandler m TypedContent
dispatchCallback name oauth2 getToken getCreds = do
    csrf <- verifySessionCSRF $ tokenSessionKey name
    onErrorResponse $ oauth2HandshakeError name
    code <- requireGetParam "code"
    manager <- authHttpManager
    oauth2' <- withCallbackAndState name oauth2 csrf
    token <- errLeft $ getToken manager oauth2' $ ExchangeToken code
    creds <- errLeft $ tryFetchCreds $ getCreds manager token
    setCredsRedirect creds
  where
    errLeft :: Show e => IO (Either e a) -> AuthHandler m a
    errLeft = either (unexpectedError name) pure <=< liftIO

-- | Handle an OAuth2 @'ErrorResponse'@
--
-- These are things coming from the OAuth2 provider such an Invalid Grant or
-- Invalid Scope and /may/ be user-actionable. We've coded them to have an
-- @'erUserMessage'@ that we are comfortable displaying to the user as part of
-- the redirect, just in case.
--
oauth2HandshakeError :: Text -> ErrorResponse -> AuthHandler m a
oauth2HandshakeError name err = do
    $(logError) $ "Handshake failure in " <> name <> " plugin: " <> tshow err
    redirectMessage $ "OAuth2 handshake failure: " <> erUserMessage err

-- | Handle an unexpected error
--
-- This would be some unexpected exception while processing the callback.
-- Therefore, the user should see an opaque message and the details go only to
-- the server logs.
--
unexpectedError :: Show e => Text -> e -> AuthHandler m a
unexpectedError name err = do
    $(logError) $ "Error in " <> name <> " OAuth2 plugin: " <> tshow err
    redirectMessage "Unexpected error logging in with OAuth2"

redirectMessage :: Text -> AuthHandler m a
redirectMessage msg = do
    toParent <- getRouteToParent
    setMessage $ toHtml msg
    redirect $ toParent LoginR

tryFetchCreds :: IO a -> IO (Either SomeException a)
tryFetchCreds f =
    (Right <$> f)
        `catch` (\(ex :: IOException) -> pure $ Left $ toException ex)
        `catch` (\(ex :: YesodOAuth2Exception) -> pure $ Left $ toException ex)

withCallbackAndState :: Text -> OAuth2 -> Text -> AuthHandler m OAuth2
withCallbackAndState name oauth2 csrf = do
    let url = PluginR name ["callback"]
    render <- getParentUrlRender
    let callbackText = render url

    callback <-
        maybe
                (liftIO
                $ throwString
                $ "Invalid callback URI: "
                <> T.unpack callbackText
                <> ". Not using an absolute Approot?"
                )
                pure
            $ fromText callbackText

    pure oauth2
        { oauthCallback = Just callback
        , oauthOAuthorizeEndpoint =
            oauthOAuthorizeEndpoint oauth2
                `withQuery` [("state", encodeUtf8 csrf)]
        }

getParentUrlRender :: MonadHandler m => m (Route (SubHandlerSite m) -> Text)
getParentUrlRender = (.) <$> getUrlRender <*> getRouteToParent

-- | Set a random, ~30-character value in the session
--
-- Some (but not all) providers decode a @+@ in the state token as a space when
-- sending it back to us. We don't expect this and fail. And if we did code for
-- it, we'd then fail on the providers that /don't/ do that.
--
-- Therefore, we just exclude @+@ in our tokens, which means this function may
-- return slightly less than 30 characters.
--
setSessionCSRF :: MonadHandler m => Text -> m Text
setSessionCSRF sessionKey = do
    csrfToken <- liftIO randomToken
    csrfToken <$ setSession sessionKey csrfToken
  where
    randomToken =
        T.filter (/= '+')
            . decodeUtf8
            . convertToBase @ByteString Base64
            <$> getRandomBytes 64

-- | Verify the callback provided the same CSRF token as in our session
verifySessionCSRF :: MonadHandler m => Text -> m Text
verifySessionCSRF sessionKey = do
    token <- requireGetParam "state"
    sessionToken <- lookupSession sessionKey
    deleteSession sessionKey

    unless (sessionToken == Just token) $ do
        $(logError)
            $ "state token does not match. "
            <> "Param: "
            <> tshow token
            <> "State: "
            <> tshow sessionToken
        permissionDenied "Invalid OAuth2 state token"

    return token

requireGetParam :: MonadHandler m => Text -> m Text
requireGetParam key = do
    m <- lookupGetParam key
    maybe errInvalidArgs return m
  where
    errInvalidArgs = invalidArgs ["The '" <> key <> "' parameter is required"]

tokenSessionKey :: Text -> Text
tokenSessionKey name = "_yesod_oauth2_" <> name

tshow :: Show a => a -> Text
tshow = T.pack . show
