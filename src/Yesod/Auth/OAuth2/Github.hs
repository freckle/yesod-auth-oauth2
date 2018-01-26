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
    ) where

import Yesod.Auth.OAuth2.Prelude

import Data.List (find)
import Data.Maybe (fromMaybe)
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
    parseJSON = withObject "GithubUser" $ \o -> GithubUser
        <$> o .: "id"
        <*> o .:? "name"
        <*> o .: "login"
        <*> o .: "avatar_url"
        <*> o .:? "location"
        <*> o .:? "email"

data GithubUserEmail = GithubUserEmail
    { githubUserEmailAddress :: Text
    , githubUserEmailPrimary :: Bool
    }

instance FromJSON GithubUserEmail where
    parseJSON = withObject "GithubUserEmail" $ \o -> GithubUserEmail
        <$> o .: "email"
        <*> o .: "primary"

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
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://github.com/login/oauth/authorize" `withQuery`
            [ scopeParam "," scopes
            ]
        , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
        , oauthCallback = Nothing
        }

fetchGithubProfile :: Manager -> OAuth2Token -> IO (Creds m)
fetchGithubProfile manager token = do
    userResult <- authGetJSON manager (accessToken token) "https://api.github.com/user"
    mailResult <- authGetJSON manager (accessToken token) "https://api.github.com/user/emails"

    case (userResult, mailResult) of
        (Right _, Right []) -> throwIO $ InvalidProfileResponse "github" "no mail address for user"
        (Right user, Right mails) -> return $ toCreds user mails token
        (Left err, _) -> throwIO $ invalidProfileResponse "github" err
        (_, Left err) -> throwIO $ invalidProfileResponse "github" err

toCreds :: GithubUser -> [GithubUserEmail] -> OAuth2Token -> Creds m
toCreds user userMails token = Creds
    { credsPlugin = "github"
    , credsIdent = T.pack $ show $ githubUserId user
    , credsExtra =
        [ ("email", githubUserEmailAddress email)
        , ("login", githubUserLogin user)
        , ("avatar_url", githubUserAvatarUrl user)
        , ("access_token", atoken $ accessToken token)
        ]
        ++ maybeExtra "name" (githubUserName user)
        ++ maybeExtra "public_email" (githubUserPublicEmail user)
        ++ maybeExtra "location" (githubUserLocation user)
    }

  where
    email = fromMaybe (head userMails) $ find githubUserEmailPrimary userMails
