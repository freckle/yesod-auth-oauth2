{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RecordWildCards #-}
module Yesod.Auth.OAuth2
    ( oauth2Url
    , authOAuth2
    ) where

import Data.Aeson (FromJSON)
import Yesod.Auth
import Yesod.Auth.OAuth2.Dispatch
import Yesod.Auth.OAuth2.Provider
import Yesod.Core (WidgetT, whamlet)

-- | Login route for a provider by name
oauth2Url :: ProviderName -> AuthRoute
oauth2Url (ProviderName name) = PluginR name ["forward"]

-- | Yesod Auth Plugin for a given Provider
--
-- Example:
--
-- > import Yesod.Auth.OAuth2
-- > import Yesod.Auth.OAuth2.Github
-- >
-- > authOAuth2 (oauth2Github defaultScopes) "CLIENT_ID" "CLIENT_SECRET"
--
authOAuth2
    :: ( FromJSON a
       , ToIdent a
       , YesodAuth m
       )
    => Provider m a
    -> ClientId
    -> ClientSecret
    -> AuthPlugin m
authOAuth2 = authOAuth2Widget $ \name toParent ->
    [whamlet|
        <a href=@{toParent $ oauth2Url name}>
            Login via #{providerName name}
    |]

-- | Same, but with custom login Widget
--
-- > import Yesod.Auth.OAuth2
-- > import Yesod.Auth.OAuth2.Github
-- >
-- > authOAuth2Widget
-- >     (\name toParent ->
-- >         [whamlet|
-- >             <a href=@{toParent $ oauth2Url name}>
-- >                 Login via #{providerName name}
-- >         |]
-- >     )
-- >     $ (oauth2Github defaultScopes) -- ...
--
authOAuth2Widget
    :: ( FromJSON a
       , ToIdent a
       , YesodAuth m
       )
    => (ProviderName -> (Route Auth -> Route m) -> WidgetT m IO ())
    -> Provider m a
    -> ClientId
    -> ClientSecret
    -> AuthPlugin m
authOAuth2Widget widget p@Provider{..} cid cs =
    AuthPlugin (providerName pName) (dispatchAuthRequest p cid cs) $ widget pName
