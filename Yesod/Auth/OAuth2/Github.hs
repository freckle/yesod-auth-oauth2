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
import Control.Applicative ((<$>), (<*>), pure)
#endif

import Control.Monad (mzero)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text)
import Yesod.Auth
import Yesod.Auth.OAuth2

import qualified Data.Text as T

data GithubUser = GithubUser
    { githubUserId :: Int
    , githubUserName :: Maybe Text
    , githubUserLogin :: Text
    , githubUserAvatarUrl :: Text
    }

instance FromJSON GithubUser where
    parseJSON (Object o) = GithubUser
        <$> o .: "id"
        <*> o .:? "name"
        <*> o .: "login"
        <*> o .: "avatar_url"

    parseJSON _ = mzero

data GithubUserEmail = GithubUserEmail
    { githubUserEmail :: Text
    }

instance FromJSON GithubUserEmail where
    parseJSON (Object o) = GithubUserEmail
        <$> o .: "email"

    parseJSON _ = mzero

data GithubProfile = GithubProfile GithubUser (Maybe GithubUserEmail)

githubProfileIdent :: GithubProfile -> Text
githubProfileIdent (GithubProfile user _) = T.pack $ show $ githubUserId user

oauth2Github :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Github = oauth2GithubScoped ["user:email"]

oauth2GithubScoped :: YesodAuth m
                   => [Text]  -- ^ Scopes to request
                   -> Text    -- ^ Client ID
                   -> Text    -- ^ Client Secret
                   -> AuthPlugin m
oauth2GithubScoped scopes = oauth2Plugin OAuth2Plugin
    { oapName = "github"
    , oapAuthEndpoint = "https://github.com/login/oauth/authorize?scope=" <> T.intercalate "," scopes
    , oapTokenEndpoint = "https://github.com/login/oauth/access_token"
    , oapFetchProfile = fetchProfile
    , oapToCredsIdent = githubProfileIdent
    , oapToCredsExtra = toCredsExtra
    }

  where
    fetchProfile manager token = do
        user <- authGetJSON manager token "https://api.github.com/user"
        emails <- authGetJSON manager token "https://api.github.com/user/emails"

        return $ case emails of
            Right (email:_) -> GithubProfile <$> user <*> pure (Just email)
            _ -> Left "user has no email"

    toCredsExtra (GithubProfile user email) =
        [ ("login", githubUserLogin user)
        , ("avatar_url", githubUserAvatarUrl user)
        ]
        ++ maybeToTuple "name" (githubUserName user)
        ++ maybeToTuple "email" (githubUserEmail <$> email)

    maybeToTuple _ Nothing = []
    maybeToTuple k (Just x) = [(k, x)]
