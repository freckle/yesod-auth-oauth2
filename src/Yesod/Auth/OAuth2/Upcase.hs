{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://upcase.com
--
-- * Authenticates against upcase
-- * Uses upcase user id as credentials identifier
--
module Yesod.Auth.OAuth2.Upcase
  ( oauth2Upcase
  ) where

import Yesod.Auth.OAuth2.Prelude

import qualified Data.Text as T

newtype User = User Int

instance FromJSON User where
  parseJSON = withObject "User" $ \root -> do
    o <- root .: "user"
    User <$> o .: "id"

pluginName :: Text
pluginName = "upcase"

oauth2Upcase :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Upcase clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <- authGetProfile
      pluginName
      manager
      token
      "http://upcase.com/api/v1/me.json"

    pure Creds { credsPlugin = pluginName
               , credsIdent  = T.pack $ show userId
               , credsExtra  = setExtra token userResponse
               }
 where
  oauth2 = OAuth2
    { oauth2ClientId          = clientId
    , oauth2ClientSecret      = Just clientSecret
    , oauth2AuthorizeEndpoint = "http://upcase.com/oauth/authorize"
    , oauth2TokenEndpoint     = "http://upcase.com/oauth/token"
    , oauth2RedirectUri       = Nothing
    }
