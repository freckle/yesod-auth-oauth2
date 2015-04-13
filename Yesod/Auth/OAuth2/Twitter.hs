{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.Twitter
    ( oauth2Twitter
    , module Yesod.Auth.OAuth2
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Yesod.Auth
import Yesod.Auth.OAuth2

data TwitterUser = TwitterUser
    { twitterUserId :: Text
    , twitterUserName :: Text
    , twitterScreenName :: Text
    }

instance FromJSON TwitterUser where
    parseJSON (Object o) = TwitterUser
        <$> o .: "id_str"
        <*> o .: "name"
        <*> o .: "screen_name"

    parseJSON _ = mzero

oauth2Twitter :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Twitter = oauth2Plugin OAuth2Plugin
    { oapName = "twitter"
    , oapAuthEndpoint = "https://api.twitter.com/oauth/authorize"
    , oapTokenEndpoint = "https://api.twitter.com/oauth/access_token"
    , oapFetchProfile = fetchProfile
    , oapToCredsIdent = twitterUserId
    , oapToCredsExtra = toCredsExtra
    }

  where
    fetchProfile manager token = authGetJSON manager token
        "https://api.twitter.com/1.1/account/verify_credentials.json"

    toCredsExtra user =
        [ ("name", twitterUserName user)
        , ("screen_name", twitterScreenName user)
        ]
