{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.ClassLink
  ( oauth2ClassLink
  , oauth2ClassLinkScoped
  ) where

import Yesod.Auth.OAuth2.Prelude

import qualified Data.Text as T

newtype User = User Int

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "UserId"

pluginName :: Text
pluginName = "classlink"

defaultScopes :: [Text]
defaultScopes = ["profile", "oneroster"]

oauth2ClassLink :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2ClassLink = oauth2ClassLinkScoped defaultScopes

oauth2ClassLinkScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2ClassLinkScoped scopes clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <- authGetProfile
      pluginName
      manager
      token
      "https://nodeapi.classlink.com/v2/my/info"

    pure Creds
      { credsPlugin = pluginName
      , credsIdent = T.pack $ show userId
      , credsExtra = setExtra token userResponse
      }
 where
  oauth2 = OAuth2
    { oauth2ClientId = clientId
    , oauth2ClientSecret = Just clientSecret
    , oauth2AuthorizeEndpoint =
      "https://launchpad.classlink.com/oauth2/v2/auth"
        `withQuery` [scopeParam "," scopes]
    , oauth2TokenEndpoint = "https://launchpad.classlink.com/oauth2/v2/token"
    , oauth2RedirectUri = Nothing
    }
