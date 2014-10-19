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
    , oauth2GithubCheckOrg
    , oauth2GithubQueried
    , module Yesod.Auth.OAuth2
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception.Lifted
import Control.Monad (mzero, void)
import Data.Aeson
import Data.Text (Text)
import Data.Map as M
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

oauth2GithubCheckOrg :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> Text -- ^ GitHub Org whose membership the user will be checked against
             -> AuthPlugin m
oauth2GithubCheckOrg clientId clientSecret githubOrg =
    oauth2GithubQueried clientId clientSecret ["user:email", "read:org"]
                        (fetchGithubProfileCheckOrg githubOrg)
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

-- the queries

fetchGithubProfile :: Manager -> AccessToken -> IO (Creds m)
fetchGithubProfile manager token = do
  (_, creds) <- queryJSONWithToken "user" (uncurry toGithubCreds) manager token
  (_, sessionMap) <- queryJSON "user/emails" toGithubCredsExtraEmail manager token
  return $ addSessionMap creds [sessionMap]

fetchGithubProfileCheckOrg :: Text -> Manager -> AccessToken -> IO (Creds m)
fetchGithubProfileCheckOrg githubOrg manager token = do
  ((_token, githubUser), creds) <- queryJSONWithToken "user" (uncurry toGithubCreds) manager token
  (_githubUserEmail, sessionMap) <- queryJSON "user/emails" toGithubCredsExtraEmail manager token
  queryWoResult ("orgs/" `mappend` githubOrg `mappend` "/members/"  `mappend` githubUserLogin githubUser) manager token
  return $ addSessionMap creds [sessionMap]

-- Functions From results of query to Creds or Session map

toGithubCreds :: AccessToken -> GithubUser -> Creds m
toGithubCreds token user = Creds "github"
               (T.pack $ show $ githubUserId user) $
               [ ("login", githubUserLogin user)
               , ("avatar_url", githubUserAvatarUrl user)
               , ("access_token", decodeUtf8 $ accessToken token)
               ] ++ maybe [] (\name -> [("name", name)]) (githubUserName user)

toGithubCredsExtraEmail :: [GithubUserEmail] -> M.Map Text Text
toGithubCredsExtraEmail [] = throw $ InvalidProfileResponse "github" "no mail address for user"
toGithubCredsExtraEmail (mail:_) = M.singleton "email" $ githubUserEmail mail

-- utilities to create a query

githubApiEndPoint :: Text
githubApiEndPoint = "https://api.github.com"

queryWoResult :: Text -> Manager -> AccessToken -> IO()
queryWoResult url manager token = do
  _ <- query url (const ()) githubAuthGetBS manager token
  return ()

queryJSON :: FromJSON b => Text -> (b -> c) -> Manager -> AccessToken -> IO (b, c)
queryJSON url toResult = query url toResult githubAuthGetJson

queryJSONWithToken :: FromJSON b => Text -> ((AccessToken, b) -> c) -> Manager -> AccessToken -> IO ((AccessToken, b), c)
queryJSONWithToken url toResult manager token =
    let githubAuthGetJsonWithToken argUrl argManager argToken = do
          result <- githubAuthGetJson argUrl argManager argToken
          return $ fmap (\res -> (argToken, res)) result
    in
    query url toResult githubAuthGetJsonWithToken manager token

githubAuthGetJson :: FromJSON b => Text -> Manager -> AccessToken -> IO (OAuth2Result b)
githubAuthGetJson url manager token =
    authGetJSON manager token (encodeUtf8 url)

githubAuthGetBS :: Text -> Manager -> AccessToken -> IO (OAuth2Result ())
githubAuthGetBS url manager token = do
    result <- authGetBS manager token (encodeUtf8 url)
    return $ void result

query :: FromJSON b
      => Text
      -> (b -> c)
      -> (Text -> Manager -> AccessToken  -> IO (OAuth2Result b))
      -> Manager -> AccessToken -> IO (b, c)
query  url toResult fQuery manager token = do
  jsonResult <- fQuery (githubApiEndPoint `mappend` "/" `mappend` url) manager token
  either
    (throwIO . InvalidProfileResponse "github")
    (\b -> return (b, toResult b))
    jsonResult

addSessionMap :: Creds m -> [M.Map Text Text] -> Creds m
addSessionMap creds credsExtraMap =
    creds { credsExtra = M.toList $ M.unions credsExtraMap }
