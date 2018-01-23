{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TupleSections #-}
-- |
--
-- Generic OAuth2 plugin for Yesod
--
-- * See Yesod.Auth.OAuth2.GitHub for example usage.
--
module Yesod.Auth.OAuth2
    ( authOAuth2
    , authOAuth2Widget
    , oauth2Url
    , fromProfileURL
    , YesodOAuth2Exception(..)
    , invalidProfileResponse
    , scopeParam
    , maybeExtra
    , module Network.OAuth.OAuth2
    , module URI.ByteString
    , module URI.ByteString.Extension
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Control.Exception.Lifted
import Control.Monad (unless)
import Control.Monad.IO.Class
import Data.Aeson (Value(..), encode)
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (encodeUtf8)
import Data.Typeable
import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2 hiding (error)
import System.Random
import URI.ByteString
import URI.ByteString.Extension
import Yesod.Auth
import Yesod.Core

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T

-- | Provider name and Aeson parse error
data YesodOAuth2Exception = InvalidProfileResponse Text BL.ByteString
    deriving (Show, Typeable)

instance Exception YesodOAuth2Exception

-- | Construct an @'InvalidProfileResponse'@ exception from an @'OAuth2Error'@
--
-- This forces the @e@ in @'OAuth2Error' e@ to parse as a JSON @'Value'@ which
-- is then re-encoded for the exception message.
--
invalidProfileResponse :: Text -> OAuth2Error Value -> YesodOAuth2Exception
invalidProfileResponse name = InvalidProfileResponse name . encode

oauth2Url :: Text -> AuthRoute
oauth2Url name = PluginR name ["forward"]

-- | Create an @'AuthPlugin'@ for the given OAuth2 provider
--
-- Presents a generic @"Login via name"@ link
--
authOAuth2 :: YesodAuth m
           => Text   -- ^ Service name
           -> OAuth2 -- ^ Service details
           -> (Manager -> OAuth2Token -> IO (Creds m))
           -- ^ This function defines how to take an @'OAuth2Token'@ and
           --   retrieve additional information about the user, to be set in the
           --   session as @'Creds'@. Usually this means a second authorized
           --   request to @api/me.json@.
           --
           --   See @'fromProfileURL'@ for an example.
           -> AuthPlugin m
authOAuth2 name = authOAuth2Widget [whamlet|Login via #{name}|] name

-- | Create an @'AuthPlugin'@ for the given OAuth2 provider
--
-- Allows passing a custom widget for the login link. See @'oauth2Eve'@ for an
-- example.
--
authOAuth2Widget :: YesodAuth m
                 => WidgetT m IO ()
                 -> Text
                 -> OAuth2
                 -> (Manager -> OAuth2Token -> IO (Creds m))
                 -> AuthPlugin m
authOAuth2Widget widget name oauth getCreds = AuthPlugin name dispatch login

  where
    url = PluginR name ["callback"]

    withCallback csrfToken = do
        tm <- getRouteToParent
        render <- lift getUrlRender
        return oauth
            { oauthCallback = Just $ unsafeFromText $ render $ tm url
            , oauthOAuthorizeEndpoint = oauthOAuthorizeEndpoint oauth
                `withQuery` [("state", encodeUtf8 csrfToken)]
            }

    dispatch "GET" ["forward"] = do
        csrfToken <- liftIO generateToken
        setSession tokenSessionKey csrfToken
        authUrl <- toText . authorizationUrl <$> withCallback csrfToken
        lift $ redirect authUrl

    dispatch "GET" ["callback"] = do
        csrfToken <- requireGetParam "state"
        oldToken <- lookupSession tokenSessionKey
        deleteSession tokenSessionKey
        unless (oldToken == Just csrfToken) $ permissionDenied "Invalid OAuth2 state token"
        code <- requireGetParam "code"
        oauth' <- withCallback csrfToken
        master <- lift getYesod
        result <- liftIO $ fetchAccessToken (authHttpManager master) oauth' (ExchangeToken code)
        case result of
            Left _ -> permissionDenied "Unable to retrieve OAuth2 token"
            Right token -> do
                creds <- liftIO $ getCreds (authHttpManager master) token
                lift $ setCredsRedirect creds
          where
              requireGetParam key = do
                  m <- lookupGetParam key
                  maybe (permissionDenied $ "'" <> key <> "' parameter not provided") return m

    dispatch _ _ = notFound

    generateToken = pack . take 30 . randomRs ('a', 'z') <$> newStdGen

    tokenSessionKey :: Text
    tokenSessionKey = "_yesod_oauth2_" <> name

    login tm = [whamlet|<a href=@{tm $ oauth2Url name}>^{widget}|]

-- | Handle the common case of fetching Profile information from a JSON endpoint
--
-- Throws @'InvalidProfileResponse'@ if JSON parsing fails
--
fromProfileURL :: FromJSON a
               => Text           -- ^ Plugin name
               -> URI            -- ^ Profile URI
               -> (a -> Creds m) -- ^ Conversion to Creds
               -> Manager -> OAuth2Token -> IO (Creds m)
fromProfileURL name url toCreds manager token = do
    result <- authGetJSON manager (accessToken token) url

    case result of
        Right profile -> return $ toCreds profile
        Left err -> throwIO $ invalidProfileResponse name err

-- | A tuple of @scope@ and the given scopes separated by a delimiter
scopeParam :: Text -> [Text] -> (ByteString, ByteString)
scopeParam d = ("scope",) . encodeUtf8 . T.intercalate d

-- | A helper for providing an optional value to credsExtra
maybeExtra :: Text -> Maybe Text -> [(Text, Text)]
maybeExtra k (Just v) = [(k, v)]
maybeExtra _ Nothing  = []
