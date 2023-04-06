{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for Azure AD using the new v2 endpoints.
--
-- * Authenticates against Azure AD
-- * Uses email as credentials identifier
--
module Yesod.Auth.OAuth2.AzureADv2
  ( oauth2AzureADv2
  , oauth2AzureADv2Scoped
  ) where

import Prelude
import Yesod.Auth.OAuth2.Prelude

import Data.String
import Data.Text (unpack)

newtype User = User Text

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "mail"

pluginName :: Text
pluginName = "azureadv2"

defaultScopes :: [Text]
defaultScopes = ["openid", "profile"]

oauth2AzureADv2
  :: YesodAuth m
  => Text
    -- ^ Tenant Id
    --
    -- If using a multi-tenant App, @common@ can be given here.
    --
  -> Text -- ^ Client Id
  -> Text -- ^ Client secret
  -> AuthPlugin m
oauth2AzureADv2 = oauth2AzureADv2Scoped defaultScopes

oauth2AzureADv2Scoped
  :: YesodAuth m
  => [Text] -- ^ Scopes
  -> Text
    -- ^ Tenant Id
    --
    -- If using a multi-tenant App, @common@ can be given here.
    --
  -> Text -- ^ Client Id
  -> Text -- ^ Client Secret
  -> AuthPlugin m
oauth2AzureADv2Scoped scopes tenantId clientId clientSecret =
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
      tenantUrl "/authorize" `withQuery` [scopeParam " " scopes]
    , oauth2TokenEndpoint = tenantUrl "/token"
    , oauth2RedirectUri = Nothing
    }

  tenantUrl path =
    fromString
      $ "https://login.microsoftonline.com/"
      <> unpack tenantId
      <> "/oauth2/v2.0"
      <> path
