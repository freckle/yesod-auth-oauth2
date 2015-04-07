{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
--
-- Generic OAuth2 plugin for Yesod
--
-- * See Yesod.Auth.OAuth2.GitHub for example usage.
--
module Yesod.Auth.OAuth2
    ( authOAuth2
    , oauth2Url
    , YesodOAuth2Exception(..)
    , module Network.OAuth.OAuth2
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>))
#endif

import Control.Exception.Lifted
import Control.Monad.IO.Class
import Data.ByteString (ByteString)
import Data.Monoid ((<>))
import Data.Text (Text, pack)
import Data.Text.Encoding (decodeUtf8With, encodeUtf8)
import Data.Text.Encoding.Error (lenientDecode)
import Data.Typeable
import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2
import System.Random
import Yesod.Auth
import Yesod.Core
import Yesod.Form

import qualified Data.ByteString.Lazy as BL

-- | Provider name and Aeson parse error
data YesodOAuth2Exception = InvalidProfileResponse Text BL.ByteString
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

    withCallback csrfToken = do
        tm <- getRouteToParent
        render <- lift getUrlRender
        return oauth
            { oauthCallback = Just $ encodeUtf8 $ render $ tm url
            , oauthOAuthorizeEndpoint = oauthOAuthorizeEndpoint oauth
                <> "&state=" <> encodeUtf8 csrfToken
            }

    dispatch "GET" ["forward"] = do
        csrfToken <- liftIO generateToken
        setSession tokenSessionKey csrfToken
        authUrl <- bsToText . authorizationUrl <$> withCallback csrfToken
        lift $ redirect authUrl

    dispatch "GET" ["callback"] = do
        newToken <- lookupGetParam "state"
        oldToken <- lookupSession tokenSessionKey
        deleteSession tokenSessionKey
        case newToken of
            Just csrfToken | newToken == oldToken -> do
                code <- lift $ runInputGet $ ireq textField "code"
                oauth' <- withCallback csrfToken
                master <- lift getYesod
                result <- liftIO $ fetchAccessToken (authHttpManager master) oauth' (encodeUtf8 code)
                case result of
                    Left _ -> permissionDenied "Unable to retreive OAuth2 token"
                    Right token -> do
                        creds <- liftIO $ getCreds (authHttpManager master) token
                        lift $ setCredsRedirect creds
            _ ->
                permissionDenied "Invalid OAuth2 state token"

    dispatch _ _ = notFound

    generateToken = pack . take 30 . randomRs ('a', 'z') <$> newStdGen

    tokenSessionKey :: Text
    tokenSessionKey = "_yesod_oauth2_" <> name

    login tm = [whamlet|
        <a href=@{tm $ oauth2Url name}>Login via #{name}
        |]

bsToText :: ByteString -> Text
bsToText = decodeUtf8With lenientDecode
