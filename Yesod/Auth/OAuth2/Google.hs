{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://www.google.com
--
-- * Authenticates against Google
-- * Uses Google user id as credentials identifier
-- * Returns given_name, family_name, email, and avatar_url as extras
--
module Yesod.Auth.OAuth2.Google
       ( oauth2Google
       , oauth2GoogleScoped
       , module Yesod.Auth.OAuth2
       ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Exception.Lifted
import Control.Monad (mzero, liftM)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Maybe (maybeToList)
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Conduit (Manager)
import Yesod.Auth
import Yesod.Auth.OAuth2

import qualified Data.Text as T

oauth2Google :: YesodAuth m
                => Text -- ^ Client ID
                -> Text -- ^ Client Secret
                -> AuthPlugin m
oauth2Google clientId clientSecret = oauth2GoogleScoped clientId clientSecret ["openid", "email"]

oauth2GoogleScoped :: YesodAuth m
                      => Text -- ^ Client ID
                      -> Text -- ^ Client Secret
                      -> [Text] -- ^ List of scopes to request
                      -> AuthPlugin m
oauth2GoogleScoped clientId clientSecret scopes = authOAuth2 "google" oauth fetchGoogleProfile
  where
    oauth = OAuth2
        { oauthClientId = encodeUtf8 clientId
        , oauthClientSecret = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint = encodeUtf8 $ "https://accounts.google.com/o/oauth2/auth?scope=" <> T.intercalate "+" scopes
        , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
        , oauthCallback = Nothing
        }


fetchGoogleProfile :: Manager -> AccessToken -> IO (Creds m)
fetchGoogleProfile manager token = do
  user <- authGetJSON manager token "https://www.googleapis.com/oauth2/v3/userinfo"
  case user of
   Right user -> return $ toCreds user token
   Left err -> throwIO $ InvalidProfileResponse "google" err

data GoogleUser = GoogleUser
                  { googleUserId :: Text
                  , googleUserName :: Text
                  , googleUserEmail :: Text
                  , googleUserPicture :: Text
                  , googleUserGivenName :: Text
                  , googleUserFamilyName :: Text
                  , googleUserHostedDomain :: Maybe Text
                  }

instance FromJSON GoogleUser where
  parseJSON (Object o) = GoogleUser
                         <$> o .: "sub"
                         <*> o .: "name"
                         <*> o .: "email"
                         <*> o .: "picture"
                         <*> o .: "given_name"
                         <*> o .: "family_name"
                         <*> o .:? "hd"

  parseJSON _ = mzero
  

toCreds :: GoogleUser -> AccessToken -> Creds m
toCreds user token = Creds { credsPlugin = "google"
                           , credsIdent = "google-uid:" <> googleUserId user
                           , credsExtra =
                             [ ("email", googleUserEmail user)
                             , ("name", googleUserName user)
                             , ("given_name", googleUserGivenName user)
                             , ("family_name", googleUserFamilyName user)
                             , ("avatar_url", googleUserPicture user)
                             , ("access_token", decodeUtf8 $ accessToken token)
                             ] ++ maybeHostedDomain
                           }
  where maybeHostedDomain = maybeToList $ ((,) "hosted_domain") `fmap` googleUserHostedDomain user
