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
import Data.Text.Encoding (encodeUtf8)
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

oauth2Twitter :: YesodAuth m
              => Text -- ^ Client ID
              -> Text -- ^ Client Secret
              -> AuthPlugin m
oauth2Twitter clientId clientSecret = authOAuth2 "twitter"
    OAuth2
        { oauthClientId = encodeUtf8 clientId
        , oauthClientSecret = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint = "https://api.twitter.com/oauth/authorize"
        , oauthAccessTokenEndpoint = "https://api.twitter.com/oauth/access_token"
        , oauthCallback = Nothing
        }
    $ fromProfileURL "twitter" "https://api.twitter.com/1.1/account/verify_credentials.json"
    $ \user -> Creds
        { credsPlugin = "twitter"
        , credsIdent = twitterUserId user
        , credsExtra =
            [ ("name", twitterUserName user)
            , ("screen_name", twitterScreenName user)
            ]
        }
