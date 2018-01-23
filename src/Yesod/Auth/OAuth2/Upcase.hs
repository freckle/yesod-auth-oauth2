{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://upcase.com
--
-- * Authenticates against upcase
-- * Uses upcase user id as credentials identifier
-- * Returns first_name, last_name, and email as extras
--
module Yesod.Auth.OAuth2.Upcase
    ( oauth2Upcase
    , module Yesod.Auth.OAuth2
    ) where

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Yesod.Auth
import Yesod.Auth.OAuth2

data UpcaseUser = UpcaseUser
    { upcaseUserId :: Int
    , upcaseUserFirstName :: Text
    , upcaseUserLastName :: Text
    , upcaseUserEmail :: Text
    }

instance FromJSON UpcaseUser where
    parseJSON (Object o) = UpcaseUser
        <$> o .: "id"
        <*> o .: "first_name"
        <*> o .: "last_name"
        <*> o .: "email"

    parseJSON _ = mzero

newtype UpcaseResponse = UpcaseResponse UpcaseUser

instance FromJSON UpcaseResponse where
    parseJSON (Object o) = UpcaseResponse
        <$> o .: "user"

    parseJSON _ = mzero

oauth2Upcase :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Upcase clientId clientSecret = authOAuth2 "upcase"
    OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "http://upcase.com/oauth/authorize"
        , oauthAccessTokenEndpoint = "http://upcase.com/oauth/token"
        , oauthCallback = Nothing
        }
    $ fromProfileURL "upcase" "http://upcase.com/api/v1/me.json"
    $ \user -> Creds
        { credsPlugin = "upcase"
        , credsIdent = T.pack $ show $ upcaseUserId user
        , credsExtra =
            [ ("first_name", upcaseUserFirstName user)
            , ("last_name", upcaseUserLastName user)
            , ("email", upcaseUserEmail user)
            ]
        }
