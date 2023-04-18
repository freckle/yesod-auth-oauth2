{-# LANGUAGE OverloadedStrings #-}

-- |
-- Module: Yesod.Auth.OAuth2.Okta
--
-- OAuth2 plugin for <https://www.okta.com/>
--
-- -- * Authenticates against a specific Okta application
-- -- * Uses Okta sub as user id
module Yesod.Auth.OAuth2.Okta
  ( oauth2Okta,
    oauth2OktaWithScopes,
    defaultOktaScopes,
    pluginName,
    User (..),
  )
where

import Data.Aeson as Aeson
import Data.ByteString (ByteString)
import Yesod.Auth.OAuth2.Prelude
import Prelude

-- | Okta User's info: https://developer.okta.com/docs/reference/api/oidc/#userinfo
newtype User = User Text

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "sub"

-- | Default scopes
defaultOktaScopes :: [Text]
defaultOktaScopes = ["openid"]

-- | Plugin name for callback routes and session data.
pluginName :: Text
pluginName = "okta"

-- | Creates an Okta 'AuthPlugin' for application using the default scopes.
oauth2Okta ::
  YesodAuth m =>
  -- | The host address of the Okta application (absolute)
  URI ->
  -- | The authorization server
  ByteString ->
  -- | Application Root for redirect links
  Maybe Text ->
  -- | Client ID of the Okta application
  Text ->
  -- | Client Secret of the Okta application
  Text ->
  AuthPlugin m
oauth2Okta = oauth2OktaWithScopes defaultOktaScopes

-- | Creates an Okta 'AuthPlugin' for application with access to the provided scopes.
oauth2OktaWithScopes ::
  YesodAuth m =>
  -- | The scopes accessible to the 'AuthPlugin'
  [Text] ->
  -- | The host address of the Okta application (absolute)
  URI ->
  -- | The authorization server
  ByteString ->
  -- | Application Root for building callbacks
  Maybe Text ->
  -- | Client ID of the Okta application
  Text ->
  -- | Client Secret of the Okta application
  Text ->
  AuthPlugin m
oauth2OktaWithScopes scopes host authorizationServer appRoot clientId clientSecret =
  authOAuth2 pluginName oauth2 $ \manager token -> do
    (User uid, userResponse) <-
      authGetProfile
        pluginName
        manager
        token
        (host `withPath` (mkEndpointSegment authorizationServer "userinfo"))
    pure
      Creds
        { credsPlugin = pluginName,
          credsIdent = uid,
          credsExtra = setExtra token userResponse
        }
  where
    oauth2 =
      OAuth2
        { oauth2ClientId = clientId,
          oauth2ClientSecret = Just clientSecret,
          oauth2AuthorizeEndpoint =
            host
              `withPath` (mkEndpointSegment authorizationServer "authorize")
              `withQuery` [scopeParam " " scopes],
          oauth2TokenEndpoint = host `withPath` (mkEndpointSegment authorizationServer "token"),
          oauth2RedirectUri = Nothing,
          oauth2AppRoot = appRoot
        }

-- | Helper function for creating an endpoint path segment for the given authorization server
-- and endpoint.
mkEndpointSegment ::
  -- | Authorization server ID
  ByteString ->
  -- | Endpoint
  ByteString ->
  ByteString
mkEndpointSegment authorizationServer endpoint =
  "/oauth2/" <> authorizationServer <> "/v1/" <> endpoint
