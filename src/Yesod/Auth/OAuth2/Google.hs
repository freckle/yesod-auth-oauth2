{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://www.google.com
--
-- * Authenticates against Google
-- * Uses Google user id or email as credentials identifier
-- * Returns given_name, family_name, email, and avatar_url as extras
--
-- Note: This may eventually replace Yesod.Auth.GoogleEmail2. Currently it
-- provides the same functionality except that GoogleEmail2 returns more profile
-- information.
--
module Yesod.Auth.OAuth2.Google
    ( oauth2Google
    , oauth2GoogleScoped
    , oauth2GoogleScopedWithCustomId
    , googleUid
    , emailUid
    , module Yesod.Auth.OAuth2
    ) where

import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text)
import Network.HTTP.Conduit (Manager)
import Yesod.Auth
import Yesod.Auth.OAuth2

-- | Auth with Google
--
-- Requests @openid@ and @email@ scopes and uses email as the @'Creds'@
-- identifier.
--
oauth2Google :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Google = oauth2GoogleScoped ["openid", "email"]

-- | Auth with Google
--
-- Requests custom scopes and uses email as the @'Creds'@ identifier.
--
oauth2GoogleScoped :: YesodAuth m
                   => [Text] -- ^ List of scopes to request
                   -> Text -- ^ Client ID
                   -> Text -- ^ Client Secret
                   -> AuthPlugin m
oauth2GoogleScoped = oauth2GoogleScopedWithCustomId emailUid

-- | Auth with Google
--
-- Requests custom scopes and uses the given function to create credentials
-- which allows for using any attribute as the identifier.
--
-- See @'emailUid'@ and @'googleUid'@.
--
oauth2GoogleScopedWithCustomId :: YesodAuth m
                               => (GoogleUser -> OAuth2Token -> Creds m)
                               -- ^ A function to generate the credentials
                               -> [Text] -- ^ List of scopes to request
                               -> Text -- ^ Client ID
                               -> Text -- ^ Client secret
                               -> AuthPlugin m
oauth2GoogleScopedWithCustomId toCreds scopes clientId clientSecret =
    authOAuth2 "google" oauth $ fetchGoogleProfile toCreds

  where
    oauth = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://accounts.google.com/o/oauth2/auth" `withQuery`
            [ scopeParam "+" scopes
            ]
        , oauthAccessTokenEndpoint = "https://www.googleapis.com/oauth2/v3/token"
        , oauthCallback = Nothing
        }

fetchGoogleProfile :: (GoogleUser -> OAuth2Token -> Creds m) -> Manager -> OAuth2Token -> IO (Creds m)
fetchGoogleProfile toCreds manager token = do
    userInfo <- authGetJSON manager (accessToken token) "https://www.googleapis.com/oauth2/v3/userinfo"
    case userInfo of
      Right user -> return $ toCreds user token
      Left err -> throwIO $ invalidProfileResponse "google" err

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

-- | Build a @'Creds'@ using the user's google-uid as the identifier
googleUid :: GoogleUser -> OAuth2Token -> Creds m
googleUid = uidBuilder $ ("google-uid:" <>) . googleUserId

-- | Build a @'Creds'@ using the user's email as the identifier
emailUid :: GoogleUser -> OAuth2Token -> Creds m
emailUid = uidBuilder googleUserEmail

uidBuilder :: (GoogleUser -> Text) -> GoogleUser -> OAuth2Token -> Creds m
uidBuilder f user token = Creds
    { credsPlugin = "google"
    , credsIdent = f user
    , credsExtra =
        [ ("email", googleUserEmail user)
        , ("name", googleUserName user)
        , ("given_name", googleUserGivenName user)
        , ("family_name", googleUserFamilyName user)
        , ("avatar_url", googleUserPicture user)
        , ("access_token", atoken $ accessToken token)
        ]
        ++ maybeExtra "hosted_domain" (googleUserHostedDomain user)
    }
