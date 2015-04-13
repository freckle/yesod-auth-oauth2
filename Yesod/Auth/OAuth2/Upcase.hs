{-# LANGUAGE CPP #-}
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

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Yesod.Auth
import Yesod.Auth.OAuth2
import qualified Data.Text as T

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

data UpcaseResponse = UpcaseResponse UpcaseUser

instance FromJSON UpcaseResponse where
    parseJSON (Object o) = UpcaseResponse
        <$> o .: "user"

    parseJSON _ = mzero

oauth2Upcase :: YesodAuth m
            => Text -- ^ Client ID
            -> Text -- ^ Client Secret
            -> AuthPlugin m
oauth2Upcase = oauth2Plugin OAuth2Plugin
    { oapName = "upcase"
    , oapAuthEndpoint = "http://upcase.com/oauth/authorize"
    , oapTokenEndpoint = "http://upcase.com/oauth/token"
    , oapFetchProfile = \manager token ->
        authGetJSON manager token "http://upcase.com/api/v1/me.json"
    , oapToCredsIdent = T.pack . show . upcaseUserId
    , oapToCredsExtra = toCredsExtra
    }

toCredsExtra :: UpcaseUser -> [(Text, Text)]
toCredsExtra user =
    [ ("first_name", upcaseUserFirstName user)
    , ("last_name" , upcaseUserLastName user)
    , ("email"     , upcaseUserEmail user)
    ]
