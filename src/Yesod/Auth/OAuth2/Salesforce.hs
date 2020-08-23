{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://login.salesforce.com
--
-- * Authenticates against Salesforce (or sandbox)
-- * Uses Salesforce user id as credentials identifier
--
module Yesod.Auth.OAuth2.Salesforce
    ( oauth2Salesforce
    , oauth2SalesforceScoped
    , oauth2SalesforceSandbox
    , oauth2SalesforceSandboxScoped
    )
where

import Yesod.Auth.OAuth2.Prelude

newtype User = User Text

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User <$> o .: "user_id"

pluginName :: Text
pluginName = "salesforce"

defaultScopes :: [Text]
defaultScopes = ["openid", "email", "api"]

oauth2Salesforce :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Salesforce = oauth2SalesforceScoped defaultScopes

oauth2SalesforceScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2SalesforceScoped = salesforceHelper
    pluginName
    "https://login.salesforce.com/services/oauth2/userinfo"
    "https://login.salesforce.com/services/oauth2/authorize"
    "https://login.salesforce.com/services/oauth2/token"

oauth2SalesforceSandbox :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2SalesforceSandbox = oauth2SalesforceSandboxScoped defaultScopes

oauth2SalesforceSandboxScoped
    :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2SalesforceSandboxScoped = salesforceHelper
    (pluginName <> "-sandbox")
    "https://test.salesforce.com/services/oauth2/userinfo"
    "https://test.salesforce.com/services/oauth2/authorize"
    "https://test.salesforce.com/services/oauth2/token"

salesforceHelper
    :: YesodAuth m
    => Text
    -> URI -- ^ User profile
    -> URI -- ^ Authorize
    -> URI -- ^ Token
    -> [Text]
    -> Text
    -> Text
    -> AuthPlugin m
salesforceHelper name profileUri authorizeUri tokenUri scopes clientId clientSecret
    = authOAuth2 name oauth2 $ \manager token -> do
        (User userId, userResponse) <- authGetProfile
            name
            manager
            token
            profileUri

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = userId
            , credsExtra = setExtra token userResponse
            }
  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint =
            authorizeUri `withQuery` [scopeParam " " scopes]
        , oauthAccessTokenEndpoint = tokenUri
        , oauthCallback = Nothing
        }
