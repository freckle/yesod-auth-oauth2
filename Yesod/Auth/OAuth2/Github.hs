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
    , oauth2GithubQueried
    , module Yesod.Auth.OAuth2
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Monoid (mappend)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Yesod.Auth
import Yesod.Auth.OAuth2
import Yesod.Core
import Yesod.Form
import Network.HTTP.Conduit(Manager)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import qualified Data.ByteString as BS
import qualified Data.Text as T

data GithubUser = GithubUser
    { githubUserId    :: Int
    , githubUserName  :: Maybe Text
    , githubUserLogin :: Text
    , githubUserAvatarUrl :: Text
    }

instance FromJSON GithubUser where
    parseJSON (Object o) =
        GithubUser <$> o .: "id"
                   <*> o .:? "name"
                   <*> o .: "login"
                   <*> o .: "avatar_url"

    parseJSON _ = mzero

data GithubUserEmail = GithubUserEmail
    { githubUserEmail :: Text
    }

instance FromJSON GithubUserEmail where
    parseJSON (Object o) =
        GithubUserEmail <$> o .: "email"

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

oauth2GithubScoped clientId clientSecret scopes =
    oauth2GithubQueried clientId clientSecret scopes fetchGithubProfile

oauth2GithubQueried :: YesodAuth m
                          => Text -- ^ Client ID
                          -> Text -- ^ Client Secret
                          -> [Text] -- ^ List of scopes to request
                          -> (Manager -> AccessToken -> IO (Creds m))
                          -- ^ These functions define how to take an @'AccessToken'@ and
                          --   retrieve additional information about the user, to be
                          --   set in the session as @'Creds'@. Usually this means a
                          --   second authorized request to @api/me.json@.
                          --   It can fail the login by throwing the @'IOException'@
                          --   @'InvalidProfileResponse'@
                          -> AuthPlugin m

oauth2GithubQueried clientId clientSecret scopes credsQuery =
    basicPlugin {apDispatch = dispatch}
    where
        oauth = OAuth2
                { oauthClientId            = encodeUtf8 clientId
                , oauthClientSecret        = encodeUtf8 clientSecret
                , oauthOAuthorizeEndpoint  = encodeUtf8 $ "https://github.com/login/oauth/authorize?scope=" `T.append` T.intercalate "," scopes
                , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                , oauthCallback            = Nothing
                }

        withState state = authOAuth2 "github"
            (oauth {oauthOAuthorizeEndpoint = oauthOAuthorizeEndpoint oauth `BS.append` "&state=" `BS.append` encodeUtf8 state})
            credsQuery  -- this argument is only used to type-check

        basicPlugin = authOAuth2 "github" oauth credsQuery

        dispatch "GET" ["forward"] = do
            state <- liftIO $ fmap (T.pack . toString) nextRandom
            setSession "githubState" state
            apDispatch (withState state) "GET" ["forward"]

        dispatch "GET" ["callback"] = do
            state <- lift $ runInputGet $ ireq textField "state"
            savedState <- lookupSession "githubState"
            _ <- apDispatch basicPlugin "GET" ["callback"]
            case savedState of
                Just saved | saved == state -> apDispatch basicPlugin "GET" ["callback"]
                Just saved -> invalidArgs ["state: " `mappend` state `mappend` ", and not: " `mappend` saved]
                _ -> invalidArgs ["state: " `mappend` state]

        dispatch method ps = apDispatch basicPlugin method ps

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
toCreds user userMail token = Creds "github"
    (T.pack $ show $ githubUserId user)
    cExtra
    where
        cExtra = [ ("email", githubUserEmail $ head userMail)
                 , ("login", githubUserLogin user)
                 , ("avatar_url", githubUserAvatarUrl user)
                 , ("access_token", decodeUtf8 $ accessToken token)
                 ] ++ (maybeName $ githubUserName user)
        maybeName Nothing     = []
        maybeName (Just name) = [("name", name)]
