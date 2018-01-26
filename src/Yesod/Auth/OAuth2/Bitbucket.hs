{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://bitbucket.com
--
-- * Authenticates against bitbucket
-- * Uses bitbucket uuid as credentials identifier
-- * Returns email, username, full name, location and avatar as extras
--
module Yesod.Auth.OAuth2.Bitbucket
    ( oauth2Bitbucket
    , oauth2BitbucketScoped
    ) where

import Yesod.Auth.OAuth2.Prelude

import Data.List (find)
import Data.Maybe (fromMaybe)
import qualified Data.Text as T

data BitbucketUser = BitbucketUser
    { bitbucketUserId :: Text
    , bitbucketUserName :: Maybe Text
    , bitbucketUserLogin :: Text
    , bitbucketUserLocation :: Maybe Text
    , bitbucketUserLinks :: BitbucketUserLinks
    }

instance FromJSON BitbucketUser where
    parseJSON = withObject "BitbucketUser" $ \o -> BitbucketUser
        <$> o .: "uuid"
        <*> o .:? "display_name"
        <*> o .: "username"
        <*> o .:? "location"
        <*> o .: "links"

newtype BitbucketUserLinks = BitbucketUserLinks
    { bitbucketAvatarLink :: BitbucketLink
    }

instance FromJSON BitbucketUserLinks where
    parseJSON = withObject "BitbucketUserLinks" $ \o -> BitbucketUserLinks
        <$> o .: "avatar"

newtype BitbucketLink = BitbucketLink
    { bitbucketLinkHref :: Text
    }

instance FromJSON BitbucketLink where
    parseJSON = withObject "BitbucketLink" $ \o -> BitbucketLink
        <$> o .: "href"

newtype BitbucketEmailSearchResults = BitbucketEmailSearchResults
    { bitbucketEmails :: [BitbucketUserEmail]
    }

instance FromJSON BitbucketEmailSearchResults where
    parseJSON = withObject "BitbucketEmailSearchResults" $ \o -> BitbucketEmailSearchResults
        <$> o .: "values"

data BitbucketUserEmail = BitbucketUserEmail
    { bitbucketUserEmailAddress :: Text
    , bitbucketUserEmailPrimary :: Bool
    }

instance FromJSON BitbucketUserEmail where
    parseJSON = withObject "BitbucketUserEmail" $ \o -> BitbucketUserEmail
        <$> o .: "email"
        <*> o .: "is_primary"

oauth2Bitbucket :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Bitbucket clientId clientSecret = oauth2BitbucketScoped clientId clientSecret ["account"]

oauth2BitbucketScoped :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> [Text] -- ^ List of scopes to request
             -> AuthPlugin m
oauth2BitbucketScoped clientId clientSecret scopes = authOAuth2 "bitbucket" oauth fetchBitbucketProfile
  where
    oauth = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://bitbucket.com/site/oauth2/authorize" `withQuery`
            [ scopeParam "," scopes
            ]
        , oauthAccessTokenEndpoint = "https://bitbucket.com/site/oauth2/access_token"
        , oauthCallback = Nothing
        }

fetchBitbucketProfile :: Manager -> OAuth2Token -> IO (Creds m)
fetchBitbucketProfile manager token = do
    userResult <- authGetJSON manager (accessToken token) "https://api.bitbucket.com/2.0/user"
    mailResult <- authGetJSON manager (accessToken token) "https://api.bitbucket.com/2.0/user/emails"

    case (userResult, mailResult) of
        (Right user, Right mails) -> return $ toCreds user (bitbucketEmails mails) token
        (Left err, _) -> throwIO $ invalidProfileResponse "bitbucket" err
        (_, Left err) -> throwIO $ invalidProfileResponse "bitbucket" err

toCreds :: BitbucketUser -> [BitbucketUserEmail] -> OAuth2Token -> Creds m
toCreds user userMails token = Creds
    { credsPlugin = "bitbucket"
    , credsIdent = T.pack $ show $ bitbucketUserId user
    , credsExtra =
        [ ("email", bitbucketUserEmailAddress email)
        , ("login", bitbucketUserLogin user)
        , ("avatar_url", bitbucketLinkHref (bitbucketAvatarLink (bitbucketUserLinks user)))
        , ("access_token", atoken $ accessToken token)
        ]
        ++ maybeExtra "name" (bitbucketUserName user)
        ++ maybeExtra "location" (bitbucketUserLocation user)
    }

  where
    email = fromMaybe (head userMails) $ find bitbucketUserEmailPrimary userMails
