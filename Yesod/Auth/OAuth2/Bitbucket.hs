{-# LANGUAGE CPP #-}
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
    , module Yesod.Auth.OAuth2
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Exception.Lifted (throwIO)
import Control.Monad (mzero)
import Data.Aeson (FromJSON, Value(Object), parseJSON, (.:), (.:?))
import Data.Maybe (fromMaybe)
import Data.List (find)
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Conduit (Manager)
import Yesod.Auth (YesodAuth, Creds(..), AuthPlugin)
import Yesod.Auth.OAuth2 (AccessToken, YesodOAuth2Exception(InvalidProfileResponse), OAuth2(..), authOAuth2, maybeExtra, accessToken, authGetJSON)

import qualified Data.Text as T

data BitbucketUser = BitbucketUser
    { bitbucketUserId :: Text
    , bitbucketUserName :: Maybe Text
    , bitbucketUserLogin :: Text
    , bitbucketUserLocation :: Maybe Text
    , bitbucketUserLinks :: BitbucketUserLinks
    }

instance FromJSON BitbucketUser where
    parseJSON (Object o) = BitbucketUser
        <$> o .: "uuid"
        <*> o .:? "display_name"
        <*> o .: "username"
        <*> o .:? "location"
        <*> o .: "links"

    parseJSON _ = mzero

data BitbucketUserLinks = BitbucketUserLinks
    { bitbucketAvatarLink :: BitbucketLink
    }

instance FromJSON BitbucketUserLinks where
    parseJSON (Object o) = BitbucketUserLinks
        <$> o .: "avatar"

    parseJSON _ = mzero

data BitbucketLink = BitbucketLink
    { bitbucketLinkHref :: Text
    }

instance FromJSON BitbucketLink where
    parseJSON (Object o) = BitbucketLink
        <$> o .: "href"
  
    parseJSON _ = mzero

data BitbucketEmailSearchResults = BitbucketEmailSearchResults
    { bitbucketEmails :: [BitbucketUserEmail]
    }

instance FromJSON BitbucketEmailSearchResults where
    parseJSON (Object o) = BitbucketEmailSearchResults
        <$> o .: "values"
  
    parseJSON _ = mzero

data BitbucketUserEmail = BitbucketUserEmail
    { bitbucketUserEmailAddress :: Text
    , bitbucketUserEmailPrimary :: Bool
    }

instance FromJSON BitbucketUserEmail where
    parseJSON (Object o) = BitbucketUserEmail
        <$> o .: "email"
        <*> o .: "is_primary"

    parseJSON _ = mzero

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
        { oauthClientId = encodeUtf8 clientId
        , oauthClientSecret = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint = encodeUtf8 $ "https://bitbucket.com/site/oauth2/authorize?scope=" <> T.intercalate "," scopes
        , oauthAccessTokenEndpoint = "https://bitbucket.com/site/oauth2/access_token"
        , oauthCallback = Nothing
        }

fetchBitbucketProfile :: Manager -> AccessToken -> IO (Creds m)
fetchBitbucketProfile manager token = do
    userResult <- authGetJSON manager token "https://api.bitbucket.com/2.0/user"
    mailResult <- authGetJSON manager token "https://api.bitbucket.com/2.0/user/emails"

    case (userResult, mailResult) of
        (Right user, Right mails) -> return $ toCreds user (bitbucketEmails mails) token
        (Left err, _) -> throwIO $ InvalidProfileResponse "bitbucket" err
        (_, Left err) -> throwIO $ InvalidProfileResponse "bitbucket" err

toCreds :: BitbucketUser -> [BitbucketUserEmail] -> AccessToken -> Creds m
toCreds user userMails token = Creds
    { credsPlugin = "bitbucket"
    , credsIdent = T.pack $ show $ bitbucketUserId user
    , credsExtra =
        [ ("email", bitbucketUserEmailAddress email)
        , ("login", bitbucketUserLogin user)
        , ("avatar_url", bitbucketLinkHref (bitbucketAvatarLink (bitbucketUserLinks user)))
        , ("access_token", decodeUtf8 $ accessToken token)
        ]
        ++ maybeExtra "name" (bitbucketUserName user)
        ++ maybeExtra "location" (bitbucketUserLocation user)
    }

  where
    email = fromMaybe (head userMails) $ find bitbucketUserEmailPrimary userMails
