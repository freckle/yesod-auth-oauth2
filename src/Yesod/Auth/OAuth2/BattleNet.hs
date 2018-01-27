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
  ( oAuth2BattleNet
  ) where

import Yesod.Auth.OAuth2.Prelude

import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T (pack, toLower)
import Yesod.Core.Widget

newtype User = User Int

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User
        <$> o .: "id"

pluginName :: Text
pluginName = "battle.net"

oAuth2BattleNet
    :: YesodAuth m
    => Text -- ^ Client ID
    -> Text -- ^ Client Secret
    -> Text -- ^ User region (e.g. "eu", "cn", "us")
    -> WidgetT m IO () -- ^ Login widget
    -> AuthPlugin m
oAuth2BattleNet clientId clientSecret region widget =
    authOAuth2Widget widget pluginName oauth2 $ \manager token -> do
        (User userId, userResponseJSON) <-
            authGetProfile pluginName manager token
                $ fromRelative "https" (apiHost $ T.toLower region) "/account/user"

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = T.pack $ show userId
            , credsExtra =
                [ ("accessToken", atoken $ accessToken token)
                , ("userResponseJSON", decodeUtf8 $ BL.toStrict userResponseJSON)
                ]
            }
  where
    host = wwwHost $ T.toLower region
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = fromRelative "https" host "/oauth/authorize"
        , oauthAccessTokenEndpoint = fromRelative "https" host "/oauth/token"
        , oauthCallback = Nothing
        }


apiHost :: Text -> Host
apiHost "cn" = "api.battlenet.com.cn"
apiHost region = Host $ encodeUtf8 $ region <> ".api.battle.net"

wwwHost :: Text -> Host
wwwHost "cn" = "www.battlenet.com.cn"
wwwHost region = Host $ encodeUtf8 $ region <> ".battle.net"
