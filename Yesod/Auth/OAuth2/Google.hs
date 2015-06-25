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
    , oauth2GoogleScopedWithCustomId
    , googleUid
    , emailUid
    , module Yesod.Auth.OAuth2
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Exception.Lifted
import Control.Monad (mzero)
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
oauth2Google = oauth2GoogleScoped ["openid", "email"]

oauth2GoogleScoped :: YesodAuth m
                      => [Text] -- ^ List of scopes to request
                      -> Text -- ^ Client ID
                      -> Text -- ^ Client Secret
                      -> AuthPlugin m
oauth2GoogleScoped = oauth2GoogleScopedWithCustomId emailUid

oauth2GoogleScopedWithCustomId :: YesodAuth m
                                  => (GoogleUser -> AccessToken -> Creds m) -- ^ A function to generate the credentials
                                  -> [Text] -- ^ List of scopes to request
                                  -> Text -- ^ Client ID
                                  -> Text -- ^ Client secret
                                  -> AuthPlugin m
oauth2GoogleScopedWithCustomId toCreds scopes clientId clientSecret = authOAuth2 "google" oauth $ fetchGoogleProfile toCreds
    where
      oauth = OAuth2
          { oauthClientId = encodeUtf8 clientId
          , oauthClientSecret = encodeUtf8 clientSecret
          , oauthOAuthorizeEndpoint = encodeUtf8 $
                                      "https://accounts.google.com/o/oauth2/auth?scope=" <> T.intercalate "+" scopes
          , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
          , oauthCallback = Nothing
          }



fetchGoogleProfile :: (GoogleUser -> AccessToken -> Creds m) -> Manager -> AccessToken -> IO (Creds m)
fetchGoogleProfile toCreds manager token = do
    userInfo <- authGetJSON manager token "https://www.googleapis.com/oauth2/v3/userinfo"
    case userInfo of
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
  

googleUid :: GoogleUser -> AccessToken -> Creds m
googleUid = uidBuilder $ ("google-uid:" <>) . googleUserId


emailUid :: GoogleUser -> AccessToken -> Creds m
emailUid = uidBuilder googleUserEmail

uidBuilder :: (GoogleUser -> Text) -> GoogleUser -> AccessToken -> Creds m
uidBuilder f user token = Creds
    { credsPlugin = "google"
    , credsIdent = f user
    , credsExtra =
        [ ("email", googleUserEmail user)
        , ("name", googleUserName user)
        , ("given_name", googleUserGivenName user)
        , ("family_name", googleUserFamilyName user)
        , ("avatar_url", googleUserPicture user)
        , ("access_token", decodeUtf8 $ accessToken token)
        ] ++ maybeHostedDomain
    }

  where
    maybeHostedDomain = maybeToList $ (,) "hosted_domain" <$> googleUserHostedDomain user
