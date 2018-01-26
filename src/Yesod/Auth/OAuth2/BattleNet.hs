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

import qualified Data.Text as T (pack, toLower)
import Yesod.Core.Widget

data BattleNetUser = BattleNetUser
    { userId :: Int
    , battleTag :: Text
    }

instance FromJSON BattleNetUser where
    parseJSON = withObject "BattleNetUser" $ \o -> BattleNetUser
        <$> o .: "id"
        <*> o .: "battletag"

oAuth2BattleNet
    :: YesodAuth m
    => Text -- ^ Client ID
    -> Text -- ^ Client Secret
    -> Text -- ^ User region (e.g. "eu", "cn", "us")
    -> WidgetT m IO () -- ^ Login widget
    -> AuthPlugin m
oAuth2BattleNet clientId clientSecret region widget =
    authOAuth2Widget widget "battle.net" oAuthData $ makeCredentials region
  where
    oAuthData = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = fromRelative "https" host "/oauth/authorize"
        , oauthAccessTokenEndpoint = fromRelative "https" host "/oauth/token"
        , oauthCallback = Nothing
        }

    host = wwwHost $ T.toLower region

makeCredentials :: Text -> Manager -> OAuth2Token -> IO (Creds m)
makeCredentials region manager token = do
    userResult <- authGetJSON manager (accessToken token)
        $ fromRelative "https" (apiHost $ T.toLower region) "/account/user"

    either
        (throwIO . invalidProfileResponse "battle.net")
        (\user ->
            return Creds
               { credsPlugin = "battle.net"
               , credsIdent = T.pack $ show $ userId user
               , credsExtra = [("battletag", battleTag user)]
               }
        ) userResult

apiHost :: Text -> Host
apiHost "cn" = "api.battlenet.com.cn"
apiHost region = Host $ encodeUtf8 $ region <> ".api.battle.net"

wwwHost :: Text -> Host
wwwHost "cn" = "www.battlenet.com.cn"
wwwHost region = Host $ encodeUtf8 $ region <> ".battle.net"
