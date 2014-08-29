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
    , module Yesod.Auth.OAuth2
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
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
    , githubUserName  :: Text
    , githubUserEmail :: Text
    , githubUserLogin :: Text
    , githubUserAvatarUrl :: Text
    }

instance FromJSON GithubUser where
    parseJSON (Object o) =
        GithubUser <$> o .: "id"
                   <*> o .: "name"
                   <*> o .: "email"
                   <*> o .: "login"
                   <*> o .: "avatar_url"

    parseJSON _ = mzero

oauth2Github :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> [Text] -- ^ List of scopes to request
             -> AuthPlugin m
oauth2Github clientId clientSecret scopes = basicPlugin {apDispatch = dispatch}
    where
        oauth = OAuth2
                { oauthClientId            = encodeUtf8 clientId
                , oauthClientSecret        = encodeUtf8 clientSecret
                , oauthOAuthorizeEndpoint  = encodeUtf8 $ "https://github.com/login/oauth/authorize?scopes=" `T.append` T.intercalate "," scopes
                , oauthAccessTokenEndpoint = "https://github.com/login/oauth/access_token"
                , oauthCallback            = Nothing
                }

        withState state = authOAuth2 "github"
            (oauth {oauthOAuthorizeEndpoint = oauthOAuthorizeEndpoint oauth `BS.append` "&state=" `BS.append` encodeUtf8 state})
            fetchGithubProfile

        basicPlugin = authOAuth2 "github" oauth fetchGithubProfile

        dispatch "GET" ["forward"] = do
            state <- liftIO $ fmap (T.pack . toString) nextRandom
            setSession "githubState" state
            apDispatch (withState state) "GET" ["forward"]

        dispatch "GET" ["callback"] = do
            state <- lift $ runInputGet $ ireq textField "state"
            savedState <- lookupSession "githubState"
            case savedState of
                Just saved | saved == state -> apDispatch basicPlugin "GET" ["callback"]
                _ -> invalidArgs ["state"]

        dispatch method ps = apDispatch basicPlugin method ps

fetchGithubProfile :: Manager -> AccessToken -> IO (Creds m)
fetchGithubProfile manager token = do
    result <- authGetJSON manager token "https://api.github.com/user"

    case result of
        Right user -> return $ toCreds user token
        Left err -> throwIO $ InvalidProfileResponse "github" err

toCreds :: GithubUser -> AccessToken -> Creds m
toCreds user token = Creds "github"
    (T.pack $ show $ githubUserId user)
    [ ("name", githubUserName user)
    , ("email", githubUserEmail user)
    , ("login", githubUserLogin user)
    , ("avatar_url", githubUserAvatarUrl user)
    , ("access_token", decodeUtf8 $ accessToken token)
    ]
