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
    ( oauth2BattleNet
    , oAuth2BattleNet
    ) where

import Yesod.Auth.OAuth2.Prelude

import qualified Data.Text as T (pack, toLower)
import Yesod.Core.Widget

newtype User = User Int

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User
        <$> o .: "id"

pluginName :: Text
pluginName = "battle.net"

oauth2BattleNet
    :: YesodAuth m
    => WidgetT m IO () -- ^ Login widget
    -> Text -- ^ User region (e.g. "eu", "cn", "us")
    -> Text -- ^ Client ID
    -> Text -- ^ Client Secret
    -> AuthPlugin m
oauth2BattleNet widget region clientId clientSecret =
    authOAuth2Widget widget pluginName oauth2 $ \manager token -> do
        (User userId, userResponse) <-
            authGetProfile pluginName manager token
                $ fromRelative "https" (apiHost $ T.toLower region) "/account/user"

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = T.pack $ show userId
            , credsExtra = setExtra token userResponse
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

oAuth2BattleNet :: YesodAuth m => Text -> Text -> Text -> WidgetT m IO () -> AuthPlugin m
oAuth2BattleNet i s r w = oauth2BattleNet w r i s
{-# DEPRECATED oAuth2BattleNet "Use oauth2BattleNet" #-}
