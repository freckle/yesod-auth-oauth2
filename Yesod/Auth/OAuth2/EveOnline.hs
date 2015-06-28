{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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
    , oauth2EveImage
    , oauth2EveScoped
    , ImageType(..)
    , module Yesod.Auth.OAuth2
    ) where

#if __GLASGOW_HASKELL__ < 710
import Control.Applicative ((<$>), (<*>))
#endif

import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Monoid ((<>))
import Data.Text as T (Text,unwords)
import Data.ByteString as B (ByteString)
import Data.Text.Encoding (encodeUtf8, decodeUtf8)
import Network.HTTP.Conduit (Manager)
import Yesod.Auth
import Yesod.Auth.OAuth2

import qualified Data.Text as T

data ImageType = BigWhite
               | SmallWhite
               | BigBlack
               | SmallBlack
               | Custom Text

data EveUser = EveUser
    { eveUserId :: Int
    , eveUserName :: Text
    , eveUserExpire :: Text
    , eveScopes :: [Text]
    , eveTokenType :: Text
    , eveCharOwnerHash :: Text
    }

instance FromJSON EveUser where
    parseJSON (Object o) = EveUser
        <$> o .: "CharacterID"
        <*> o .: "CharacterName"
        <*> o .: "ExpiresOn"
        <*> (T.words <$> o .: "Scopes")
        <*> o .: "TokenType"
        <*> o .: "CharacterOwnerHash"

    parseJSON _ = mzero

oauth2Eve :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Eve clientId clientSecret = oauth2EveScoped clientId clientSecret ["publicData"] Nothing

oauth2EveImage :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> ImageType
             -> AuthPlugin m
oauth2EveImage clientId clientSecret im = oauth2EveScoped clientId clientSecret ["publicData"] (Just . toURI $ im)
             where
               toURI :: ImageType -> Text
               toURI BigWhite = "https://images.contentful.com/idjq7aai9ylm/4PTzeiAshqiM8osU2giO0Y/5cc4cb60bac52422da2e45db87b6819c/EVE_SSO_Login_Buttons_Large_White.png?w=270&h=45"
               toURI BigBlack = "https://images.contentful.com/idjq7aai9ylm/4fSjj56uD6CYwYyus4KmES/4f6385c91e6de56274d99496e6adebab/EVE_SSO_Login_Buttons_Large_Black.png?w=270&h=45"
               toURI SmallWhite = "https://images.contentful.com/idjq7aai9ylm/18BxKSXCymyqY4QKo8KwKe/c2bdded6118472dd587c8107f24104d7/EVE_SSO_Login_Buttons_Small_White.png?w=195&h=30"
               toURI SmallBlack = "https://images.contentful.com/idjq7aai9ylm/12vrPsIMBQi28QwCGOAqGk/33234da7672c6b0cdca394fc8e0b1c2b/EVE_SSO_Login_Buttons_Small_Black.png?w=195&h=30"
               toURI (Custom a) = a

oauth2EveScoped :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> [Text] -- ^ List of scopes to request
             -> Maybe Text -- ^ Login-Image
             -> AuthPlugin m
oauth2EveScoped clientId clientSecret scopes = authOAuth2Image "eveonline" oauth fetchEveProfile
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
toCreds user token = Creds
    { credsPlugin = "eveonline"
    , credsIdent = T.pack $ show $ eveCharOwnerHash user
    , credsExtra =
        [ ("charName", eveUserName user)
        , ("tokenType", eveTokenType user)
        , ("expires", eveUserExpire user)
        ]
    }
