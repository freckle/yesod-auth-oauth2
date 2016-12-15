{-# LANGUAGE CPP               #-}
{-# LANGUAGE OverloadedStrings #-}

-- |
--
-- OAuth2 plugin for Battle.Net
--
-- * Authenticates against battle.net.
-- * Uses user's id as credentials identifier.
-- * Returns user's battletag in extras.
--

module Yesod.Auth.OAuth2.BattleNet
  ( oAuth2BattleNet )
  where

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative  ((<$>), (<*>))
#endif

import           Control.Exception    (throwIO)
import           Control.Monad        (mzero)

import           Yesod.Auth
import           Yesod.Auth.OAuth2

import           Data.Monoid          ((<>))
import           Network.HTTP.Conduit (Manager)

import           Data.Aeson
import           Data.Text            (Text)
import qualified Data.Text            as T (pack, toLower)
import qualified Data.Text.Encoding   as E (encodeUtf8)
import           Prelude
import           Yesod.Core.Widget

data BattleNetUser = BattleNetUser
              { userId    :: Int
              , battleTag :: Text
              }

instance FromJSON BattleNetUser where
  parseJSON (Object o) = BattleNetUser
                           <$> o .: "id"
                           <*> o .: "battletag"
  parseJSON _ = mzero

oAuth2BattleNet :: YesodAuth m
                => Text -- ^ Client ID
                -> Text -- ^ Client Secret
                -> Text -- ^ User region (e.g. "eu", "cn", "us")
                -> WidgetT m IO () -- ^ Login widget
                -> AuthPlugin m
oAuth2BattleNet clientId clientSecret region widget = authOAuth2Widget widget "battle.net" oAuthData (makeCredentials region)
  where oAuthData = OAuth2 { oauthClientId            = E.encodeUtf8 clientId
                           , oauthClientSecret        = E.encodeUtf8 clientSecret
                           , oauthOAuthorizeEndpoint  = E.encodeUtf8 ("https://" <> host <> "/oauth/authorize")
                           , oauthAccessTokenEndpoint = E.encodeUtf8 ("https://" <> host <> "/oauth/token")
                           , oauthCallback            = Nothing
                           }
        host = let r = T.toLower region in
                 case r of
                   "cn" -> "www.battlenet.com.cn"
                   _    -> r <> ".battle.net"

makeCredentials :: Text -> Manager -> AccessToken -> IO (Creds m)
makeCredentials region manager token = do
    userResult <- authGetJSON manager token ("https://" <> host <> "/account/user") :: IO (OAuth2Result BattleNetUser)
    case userResult of
         Left err -> throwIO $ InvalidProfileResponse "battle.net" err
         Right user -> return Creds
           { credsPlugin = "battle.net"
           , credsIdent  = T.pack $ show $ userId user
           , credsExtra  = [("battletag", battleTag user)]
           }
  where host :: URI
        host = let r = T.toLower region in
                 case r of
                   "cn" -> "api.battlenet.com.cn"
                   _    -> E.encodeUtf8 r <> ".api.battle.net"


