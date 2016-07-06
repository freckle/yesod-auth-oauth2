{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://github.com
--
-- * Authenticates against github
-- * Uses github user id as credentials identifier
-- * Returns first_name, last_name, and email as extras
--
module Yesod.Auth.OAuth2.Github
    ( oauth2Github
    , oauth2GithubScoped
    , module Yesod.Auth.OAuth2
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Conduit (Manager)
import Yesod.Auth
import Yesod.Auth.OAuth2

import qualified Data.Text as T

data GithubUser = GithubUser
    { githubUserId :: Int
    , githubUserName :: Maybe Text
    , githubUserLogin :: Text
    , githubUserAvatarUrl :: Text
    , githubUserLocation :: Maybe Text
    , githubUserPublicEmail :: Maybe Text
    }

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
        <$> o .: "id"
        <*> o .:? "name"
        <*> o .: "login"
        <*> o .: "avatar_url"
        <*> o .:? "location"
        <*> o .:? "email"

    parseJSON _ = mzero

data GithubUserEmail = GithubUserEmail
    { githubUserEmailAddress :: Text
    , githubUserEmailPrimary :: Bool
    }

instance FromJSON GithubUserEmail where
    parseJSON (Object o) = GithubUserEmail
        <$> o .: "email"
        <*> o .: "primary"

    parseJSON _ = mzero

oauth2Github :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Github clientId clientSecret = oauth2GithubScoped clientId clientSecret ["user:email"]

oauth2GithubScoped :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> [Text] -- ^ List of scopes to request
             -> AuthPlugin m
oauth2GithubScoped clientId clientSecret scopes = authOAuth2 "github" oauth fetchGithubProfile
  where
    oauth = OAuth2
        { oauthClientId = encodeUtf8 clientId
        , oauthClientSecret = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint = encodeUtf8 $ "https://github.com/login/oauth/authorize?scope=" <> T.intercalate "," scopes
        , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
        , oauthCallback = Nothing
        }

fetchGithubProfile :: Manager -> AccessToken -> IO (Creds m)
fetchGithubProfile manager token = do
    userResult <- authGetJSON manager token "https://api.github.com/user"
    mailResult <- authGetJSON manager token "https://api.github.com/user/emails"

    case (userResult, mailResult) of
        (Right _, Right []) -> throwIO $ InvalidProfileResponse "github" "no mail address for user"
        (Right user, Right mails) -> return $ toCreds user mails token
        (Left err, _) -> throwIO $ InvalidProfileResponse "github" err
        (_, Left err) -> throwIO $ InvalidProfileResponse "github" err

toCreds :: GithubUser -> [GithubUserEmail] -> AccessToken -> Creds m
toCreds user userMails token = Creds
    { credsPlugin = "github"
    , credsIdent = T.pack $ show $ githubUserId user
    , credsExtra =
        [ ("email", githubUserEmailAddress email)
        , ("login", githubUserLogin user)
        , ("avatar_url", githubUserAvatarUrl user)
        , ("access_token", decodeUtf8 $ accessToken token)
        ]
        ++ maybeExtra "name" (githubUserName user)
        ++ maybeExtra "public_email" (githubUserPublicEmail user)
        ++ maybeExtra "location" (githubUserLocation user)
    }

  where
    email = fromMaybe (head userMails) $ find githubUserEmailPrimary userMails
