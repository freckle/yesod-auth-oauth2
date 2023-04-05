{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for Azure AD.
--
-- * Authenticates against Azure AD
-- * Uses email as credentials identifier
--
module Yesod.Auth.OAuth2.AzureAD
  ( oauth2AzureAD
  , oauth2AzureADScoped
  ) where

import Prelude
import Yesod.Auth.OAuth2.Prelude

newtype User = User Text

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "mail"

pluginName :: Text
pluginName = "azuread"

defaultScopes :: [Text]
defaultScopes = ["openid", "profile"]

oauth2AzureAD :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2AzureAD = oauth2AzureADScoped defaultScopes

oauth2AzureADScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2AzureADScoped scopes clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (User userId, userResponse) <- authGetProfile
      pluginName
      manager
      token
      "https://graph.microsoft.com/v1.0/me"

    pure Creds
      { credsPlugin = pluginName
      , credsIdent = userId
      , credsExtra = setExtra token userResponse
      }
 where
  oauth2 = OAuth2
    { oauth2ClientId = clientId
    , oauth2ClientSecret = Just clientSecret
    , oauth2AuthorizeEndpoint =
      "https://login.windows.net/common/oauth2/authorize"
        `withQuery` [ scopeParam "," scopes
                    , ("resource", "https://graph.microsoft.com")
                    ]
    , oauth2TokenEndpoint = "https://login.windows.net/common/oauth2/token"
    , oauth2RedirectUri = Nothing
    }
