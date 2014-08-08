{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://learn.thoughtbot.com
--
-- * Authenticates against learn
-- * Uses learn user id as credentials identifier
-- * Returns first_name, last_name, and email as extras
--
module Yesod.Auth.OAuth2.Learn
    ( oauth2Learn
    , module Yesod.Auth.OAuth2
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Yesod.Auth
import Yesod.Auth.OAuth2
import Network.HTTP.Client (newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import qualified Data.Text as T

data LearnUser = LearnUser
    { learnUserId        :: Int
    , learnUserFirstName :: Text
    , learnUserLastName  :: Text
    , learnUserEmail     :: Text
    }

instance FromJSON LearnUser where
    parseJSON (Object o) =
        LearnUser <$> o .: "id"
                  <*> o .: "first_name"
                  <*> o .: "last_name"
                  <*> o .: "email"

    parseJSON _ = mzero

data LearnResponse = LearnResponse LearnUser

instance FromJSON LearnResponse where
    parseJSON (Object o) =
        LearnResponse <$> o .: "user"

    parseJSON _ = mzero

oauth2Learn :: YesodAuth m
            => Text -- ^ Client ID
            -> Text -- ^ Client Secret
            -> AuthPlugin m
oauth2Learn clientId clientSecret = authOAuth2 "learn"
    (OAuth2
        { oauthClientId            = encodeUtf8 clientId
        , oauthClientSecret        = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint  = "http://learn.thoughtbot.com/oauth/authorize"
        , oauthAccessTokenEndpoint = "http://learn.thoughtbot.com/oauth/token"
        , oauthCallback            = Nothing
        })
    fetchLearnProfile

fetchLearnProfile :: AccessToken -> IO (Creds m)
fetchLearnProfile token = do
    manager <- newManager tlsManagerSettings
    result <- authGetJSON manager token "http://learn.thoughtbot.com/api/v1/me.json"

    case result of
        Right (LearnResponse user) -> return $ toCreds user
        Left err -> throwIO $ InvalidProfileResponse "learn" err

toCreds :: LearnUser -> Creds m
toCreds user = Creds "learn"
    (T.pack $ show $ learnUserId user)
    [ ("first_name", learnUserFirstName user)
    , ("last_name" , learnUserLastName user)
    , ("email"     , learnUserEmail user)
    ]
