{-# LANGUAGE OverloadedStrings #-}
-- | OAuth callback error response
--
-- <https://tools.ietf.org/html/rfc6749#section-4.1.2.1>
--
module Yesod.Auth.OAuth2.ErrorResponse
    ( ErrorResponse(..)
    , erUserMessage
    , ErrorName(..)
    , onErrorResponse
    , unknownError
    )
where

import Data.Foldable (traverse_)
import Data.Text (Text)
import Data.Traversable (for)
import Yesod.Core (MonadHandler, lookupGetParam)

data ErrorName
    = InvalidRequest
    | UnauthorizedClient
    | AccessDenied
    | UnsupportedResponseType
    | InvalidScope
    | ServerError
    | TemporarilyUnavailable
    | Unknown Text
    deriving Show

data ErrorResponse = ErrorResponse
    { erName :: ErrorName
    , erDescription :: Maybe Text
    , erURI :: Maybe Text
    }
    deriving Show

-- | Textual value suitable for display to a User
erUserMessage :: ErrorResponse -> Text
erUserMessage err = case erName err of
    InvalidRequest -> "Invalid request"
    UnauthorizedClient -> "Unauthorized client"
    AccessDenied -> "Access denied"
    UnsupportedResponseType -> "Unsupported response type"
    InvalidScope -> "Invalid scope"
    ServerError -> "Server error"
    TemporarilyUnavailable -> "Temporarily unavailable"
    Unknown _ -> "Unknown error"

unknownError :: Text -> ErrorResponse
unknownError x = ErrorResponse
    { erName = Unknown x
    , erDescription = Nothing
    , erURI = Nothing
    }

-- | Check query parameters for an error, if found run the given action
--
-- The action is expected to use a short-circuit response function like
-- @'permissionDenied'@, hence this returning @()@.
--
onErrorResponse :: MonadHandler m => (ErrorResponse -> m a) -> m ()
onErrorResponse f = traverse_ f =<< checkErrorResponse

checkErrorResponse :: MonadHandler m => m (Maybe ErrorResponse)
checkErrorResponse = do
    merror <- lookupGetParam "error"

    for merror $ \err -> ErrorResponse
        <$> pure (readErrorName err)
        <*> lookupGetParam "error_description"
        <*> lookupGetParam "error_uri"

readErrorName :: Text -> ErrorName
readErrorName "invalid_request" = InvalidRequest
readErrorName "unauthorized_client" = UnauthorizedClient
readErrorName "access_denied" = AccessDenied
readErrorName "unsupported_response_type" = UnsupportedResponseType
readErrorName "invalid_scope" = InvalidScope
readErrorName "server_error" = ServerError
readErrorName "temporarily_unavailable" = TemporarilyUnavailable
readErrorName x = Unknown x
