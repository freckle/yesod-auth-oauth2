{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.Orcid
  ( oauth2Orcid
  ) where

import qualified Data.Text as T
import Yesod.Auth.OAuth2.Prelude

pluginName :: Text
pluginName = "orcid"

newtype User = User Text

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "sub"

oauth2Orcid
  :: YesodAuth m
  => Text
  -- ^ Client Id
  -> Text
  -- ^ Client Secret
  -> AuthPlugin m
oauth2Orcid clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <-
      authGetProfile
        pluginName
        manager
        token
        "https://orcid.org/oauth/userinfo"

    pure
      Creds
        { credsPlugin = pluginName
        , credsIdent = T.pack $ show userId
        , credsExtra = setExtra token userResponse
        }
 where
  oauth2 =
    OAuth2
      { oauth2ClientId = clientId
      , oauth2ClientSecret = Just clientSecret
      , oauth2AuthorizeEndpoint =
          "https://orcid.org/oauth/authorize"
            `withQuery` [scopeParam " " ["openid"]]
      , oauth2TokenEndpoint = "https://orcid.org/oauth/token"
      , oauth2RedirectUri = Nothing
      }
