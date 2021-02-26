{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Auth.OAuth2.Dispatch
    ( FetchToken
    , fetchAccessToken
    , fetchAccessToken2
    , FetchCreds
    , dispatchAuthRequest
    ) where

import Control.Monad.Except
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2
import Network.OAuth.OAuth2.TokenRequest (Errors)
import UnliftIO.Exception
import URI.ByteString.Extension
import Yesod.Auth hiding (ServerError)
import Yesod.Auth.OAuth2.DispatchError
import Yesod.Auth.OAuth2.ErrorResponse
import Yesod.Auth.OAuth2.Random
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
    handleDispatchError $ dispatchForward name oauth2
dispatchAuthRequest name oauth2 getToken getCreds "GET" ["callback"] =
    handleDispatchError $ dispatchCallback name oauth2 getToken getCreds
dispatchAuthRequest _ _ _ _ _ _ = notFound

-- | Handle @GET \/forward@
--
-- 1. Set a random CSRF token in our session
-- 2. Redirect to the Provider's authorization URL
--
dispatchForward
    :: (MonadError DispatchError m, MonadAuthHandler site m)
    => Text
    -> OAuth2
    -> m TypedContent
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
    :: (MonadError DispatchError m, MonadAuthHandler site m)
    => Text
    -> OAuth2
    -> FetchToken
    -> FetchCreds site
    -> m TypedContent
dispatchCallback name oauth2 getToken getCreds = do
    onErrorResponse $ throwError . OAuth2HandshakeError
    csrf <- verifySessionCSRF $ tokenSessionKey name
    code <- requireGetParam "code"
    manager <- authHttpManager
    oauth2' <- withCallbackAndState name oauth2 csrf
    token <- either (throwError . OAuth2ResultError) pure
        =<< liftIO (getToken manager oauth2' $ ExchangeToken code)
    creds <-
        liftIO (getCreds manager token)
        `catch` (throwError . FetchCredsIOException)
        `catch` (throwError . FetchCredsYesodOAuth2Exception)
    setCredsRedirect creds

withCallbackAndState
    :: (MonadError DispatchError m, MonadAuthHandler site m)
    => Text
    -> OAuth2
    -> Text
    -> m OAuth2
withCallbackAndState name oauth2 csrf = do
    uri <- ($ PluginR name ["callback"]) <$> getParentUrlRender
    callback <- maybe (throwError $ InvalidCallbackUri uri) pure $ fromText uri
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
    where randomToken = T.filter (/= '+') <$> randomText 64

-- | Verify the callback provided the same CSRF token as in our session
verifySessionCSRF
    :: (MonadError DispatchError m, MonadHandler m) => Text -> m Text
verifySessionCSRF sessionKey = do
    token <- requireGetParam "state"
    sessionToken <- lookupSession sessionKey
    deleteSession sessionKey
    token <$ unless
        (sessionToken == Just token)
        (throwError $ InvalidStateToken sessionToken token)

requireGetParam
    :: (MonadError DispatchError m, MonadHandler m) => Text -> m Text
requireGetParam key =
    maybe (throwError $ MissingParameter key) pure =<< lookupGetParam key

tokenSessionKey :: Text -> Text
tokenSessionKey name = "_yesod_oauth2_" <> name
