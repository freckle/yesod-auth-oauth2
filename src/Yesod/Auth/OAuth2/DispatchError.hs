{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Yesod.Auth.OAuth2.DispatchError
  ( DispatchError(..)
  , handleDispatchError
  , onDispatchError
  ) where

import Control.Monad.Except
import Data.Text (Text, pack)
import Network.OAuth.OAuth2.Compat (Errors)
import UnliftIO.Except ()
import UnliftIO.Exception
import Yesod.Auth hiding (ServerError)
import Yesod.Auth.OAuth2.ErrorResponse
import Yesod.Auth.OAuth2.Exception
import Yesod.Auth.OAuth2.Random
import Yesod.Core hiding (ErrorResponse)

data DispatchError
    = MissingParameter Text
    | InvalidStateToken (Maybe Text) Text
    | InvalidCallbackUri Text
    | OAuth2HandshakeError ErrorResponse
    | OAuth2ResultError Errors
    | FetchCredsIOException IOException
    | FetchCredsYesodOAuth2Exception YesodOAuth2Exception
    | OtherDispatchError Text
    deriving stock Show
    deriving anyclass Exception

-- | User-friendly message for any given 'DispatchError'
--
-- Most of these are opaque to the user. The exception details are present for
-- the server logs.
--
dispatchErrorMessage :: DispatchError -> Text
dispatchErrorMessage = \case
  MissingParameter name ->
    "Parameter '" <> name <> "' is required, but not present in the URL"
  InvalidStateToken{} -> "State token is invalid, please try again"
  InvalidCallbackUri{} ->
    "Callback URI was not valid, this server may be misconfigured (no approot)"
  OAuth2HandshakeError er -> "OAuth2 handshake failure: " <> erUserMessage er
  OAuth2ResultError{}              -> "Login failed, please try again"
  FetchCredsIOException{}          -> "Login failed, please try again"
  FetchCredsYesodOAuth2Exception{} -> "Login failed, please try again"
  OtherDispatchError{}             -> "Login failed, please try again"

handleDispatchError
  :: MonadAuthHandler site m
  => ExceptT DispatchError m TypedContent
  -> m TypedContent
handleDispatchError f = do
  result <- runExceptT f
  either onDispatchError pure result

onDispatchError :: MonadAuthHandler site m => DispatchError -> m TypedContent
onDispatchError err = do
  errorId <- liftIO $ randomText 16
  let suffix = " [errorId=" <> errorId <> "]"
  $(logError) $ pack (displayException err) <> suffix

  let message = dispatchErrorMessage err <> suffix
      messageValue =
        object ["error" .= object ["id" .= errorId, "message" .= message]]

  loginR <- ($ LoginR) <$> getRouteToParent

  selectRep $ do
    provideRep @_ @Html $ onErrorHtml loginR message
    provideRep @_ @Value $ pure messageValue
