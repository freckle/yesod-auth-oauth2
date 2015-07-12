{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
-- |
--
-- OAuth2 plugin for http://eveonline.com
--
-- * Authenticates against eveonline
-- * Uses EVEs unique account-user-char-hash as credentials identifier
-- * Returns charName, tokenType and expires as extras
--
module Yesod.Auth.OAuth2.EveOnline
    ( oauth2Eve
    , oauth2EveScoped
    , WidgetType(..)
    , module Yesod.Auth.OAuth2
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit (Manager)
import Yesod.Auth
import Yesod.Auth.OAuth2
import Yesod.Core.Widget

import qualified Data.Text as T

data WidgetType m
    = Plain -- ^ Simple "Login via eveonline" text
    | BigWhite
    | SmallWhite
    | BigBlack
    | SmallBlack
    | Custom (WidgetT m IO ())

data EveUser = EveUser
    { eveUserName :: Text
    , eveUserExpire :: Text
    , eveTokenType :: Text
    , eveCharOwnerHash :: Text
    }

instance FromJSON EveUser where
    parseJSON (Object o) = EveUser
        <$> o .: "CharacterName"
        <*> o .: "ExpiresOn"
        <*> o .: "TokenType"
        <*> o .: "CharacterOwnerHash"

    parseJSON _ = mzero

oauth2Eve :: YesodAuth m
          => Text -- ^ Client ID
          -> Text -- ^ Client Secret
          -> WidgetType m
          -> AuthPlugin m
oauth2Eve clientId clientSecret = oauth2EveScoped clientId clientSecret ["publicData"] . asWidget

  where
    asWidget :: YesodAuth m => WidgetType m -> WidgetT m IO ()
    asWidget Plain = [whamlet|Login via eveonline|]
    asWidget BigWhite = [whamlet|<img src="https://images.contentful.com/idjq7aai9ylm/4PTzeiAshqiM8osU2giO0Y/5cc4cb60bac52422da2e45db87b6819c/EVE_SSO_Login_Buttons_Large_White.png?w=270&h=45">|]
    asWidget BigBlack = [whamlet|<img src="https://images.contentful.com/idjq7aai9ylm/4fSjj56uD6CYwYyus4KmES/4f6385c91e6de56274d99496e6adebab/EVE_SSO_Login_Buttons_Large_Black.png?w=270&h=45">|]
    asWidget SmallWhite = [whamlet|<img src="https://images.contentful.com/idjq7aai9ylm/18BxKSXCymyqY4QKo8KwKe/c2bdded6118472dd587c8107f24104d7/EVE_SSO_Login_Buttons_Small_White.png?w=195&h=30">|]
    asWidget SmallBlack = [whamlet|<img src="https://images.contentful.com/idjq7aai9ylm/12vrPsIMBQi28QwCGOAqGk/33234da7672c6b0cdca394fc8e0b1c2b/EVE_SSO_Login_Buttons_Small_Black.png?w=195&h=30">|]
    asWidget (Custom a) = a

oauth2EveScoped :: YesodAuth m
                => Text -- ^ Client ID
                -> Text -- ^ Client Secret
                -> [Text] -- ^ List of scopes to request
                -> WidgetT m IO () -- ^ Login widget
                -> AuthPlugin m
oauth2EveScoped clientId clientSecret scopes widget =
    authOAuth2Widget widget "eveonline" oauth fetchEveProfile

  where
    oauth = OAuth2
        { oauthClientId = encodeUtf8 clientId
        , oauthClientSecret = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint = encodeUtf8 $ "https://login.eveonline.com/oauth/authorize?response_type=code&scope=" <> T.intercalate " " scopes
        , oauthAccessTokenEndpoint = "https://login.eveonline.com/oauth/token"
        , oauthCallback = Nothing
        }

fetchEveProfile :: Manager -> AccessToken -> IO (Creds m)
fetchEveProfile manager token = do
    userResult <- authGetJSON manager token "https://login.eveonline.com/oauth/verify"

    case userResult of
        Right user -> return $ toCreds user token
        Left err-> throwIO $ InvalidProfileResponse "eveonline" err

toCreds :: EveUser -> AccessToken -> Creds m
toCreds user _ = Creds
    { credsPlugin = "eveonline"
    , credsIdent = T.pack $ show $ eveCharOwnerHash user
    , credsExtra =
        [ ("charName", eveUserName user)
        , ("tokenType", eveTokenType user)
        , ("expires", eveUserExpire user)
        ]
    }
