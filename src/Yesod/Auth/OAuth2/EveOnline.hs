{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
--
-- OAuth2 plugin for http://eveonline.com
--
-- * Authenticates against eveonline
-- * Uses EVEs unique account-user-char-hash as credentials identifier
--
module Yesod.Auth.OAuth2.EveOnline
  ( oauth2Eve
  , oauth2EveScoped
  , WidgetType(..)
  ) where

import Yesod.Auth.OAuth2.Prelude

import qualified Data.Text as T
import Yesod.Core.Widget

newtype User = User Text

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "CharacterOwnerHash"

data WidgetType m
    = Plain -- ^ Simple "Login via eveonline" text
    | BigWhite
    | SmallWhite
    | BigBlack
    | SmallBlack
    | Custom (WidgetFor m ())

asWidget :: YesodAuth m => WidgetType m -> WidgetFor m ()
asWidget Plain = [whamlet|Login via eveonline|]
asWidget BigWhite =
  [whamlet|<img src="https://images.contentful.com/idjq7aai9ylm/4PTzeiAshqiM8osU2giO0Y/5cc4cb60bac52422da2e45db87b6819c/EVE_SSO_Login_Buttons_Large_White.png?w=270&h=45">|]
asWidget BigBlack
  = [whamlet|<img src="https://images.contentful.com/idjq7aai9ylm/4fSjj56uD6CYwYyus4KmES/4f6385c91e6de56274d99496e6adebab/EVE_SSO_Login_Buttons_Large_Black.png?w=270&h=45">|]
asWidget SmallWhite
  = [whamlet|<img src="https://images.contentful.com/idjq7aai9ylm/18BxKSXCymyqY4QKo8KwKe/c2bdded6118472dd587c8107f24104d7/EVE_SSO_Login_Buttons_Small_White.png?w=195&h=30">|]
asWidget SmallBlack
  = [whamlet|<img src="https://images.contentful.com/idjq7aai9ylm/12vrPsIMBQi28QwCGOAqGk/33234da7672c6b0cdca394fc8e0b1c2b/EVE_SSO_Login_Buttons_Small_Black.png?w=195&h=30">|]
asWidget (Custom a) = a

pluginName :: Text
pluginName = "eveonline"

defaultScopes :: [Text]
defaultScopes = ["publicData"]

oauth2Eve :: YesodAuth m => WidgetType m -> Text -> Text -> AuthPlugin m
oauth2Eve = oauth2EveScoped defaultScopes

oauth2EveScoped
  :: YesodAuth m => [Text] -> WidgetType m -> Text -> Text -> AuthPlugin m
oauth2EveScoped scopes widgetType clientId clientSecret =
  authOAuth2Widget (asWidget widgetType) pluginName oauth2 $ \manager token ->
    do
      (User userId, userResponse) <- authGetProfile
        pluginName
        manager
        token
        "https://login.eveonline.com/oauth/verify"

      pure Creds { credsPlugin = "eveonline"
          -- FIXME: Preserved bug. See similar comment in Bitbucket provider.
                 , credsIdent  = T.pack $ show userId
                 , credsExtra  = setExtra token userResponse
                 }
 where
  oauth2 = OAuth2
    { oauth2ClientId          = clientId
    , oauth2ClientSecret      = Just clientSecret
    , oauth2AuthorizeEndpoint = "https://login.eveonline.com/oauth/authorize"
                                  `withQuery` [ ("response_type", "code")
                                              , scopeParam " " scopes
                                              ]
    , oauth2TokenEndpoint     = "https://login.eveonline.com/oauth/token"
    , oauth2RedirectUri       = Nothing
    }
