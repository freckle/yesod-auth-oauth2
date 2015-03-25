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

import Control.Applicative ((<$>), (<*>))
import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Yesod.Auth
import Yesod.Auth.OAuth2
import Network.HTTP.Conduit(Manager)
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
oauth2Upcase clientId clientSecret = authOAuth2 "upcase"
    OAuth2
        { oauthClientId = encodeUtf8 clientId
        , oauthClientSecret = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint = "http://upcase.com/oauth/authorize"
        , oauthAccessTokenEndpoint = "http://upcase.com/oauth/token"
        , oauthCallback = Nothing
        }
    fetchUpcaseProfile

fetchUpcaseProfile :: Manager -> AccessToken -> IO (Creds m)
fetchUpcaseProfile manager token = do
    result <- authGetJSON manager token "http://upcase.com/api/v1/me.json"

    case result of
        Right (UpcaseResponse user) -> return $ toCreds user
        Left err -> throwIO $ InvalidProfileResponse "upcase" err

toCreds :: UpcaseUser -> Creds m
toCreds user = Creds
    { credsPlugin = "upcase"
    , credsIdent = T.pack $ show $ upcaseUserId user
    , credsExtra =
        [ ("first_name", upcaseUserFirstName user)
        , ("last_name" , upcaseUserLastName user)
        , ("email"     , upcaseUserEmail user)
        ]
    }
