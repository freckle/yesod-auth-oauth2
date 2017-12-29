{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.BattleNet
  ( oauth2BattleNet
  ) where

import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import URI.ByteString (Host(..))
import URI.ByteString.Extension (fromRelative)
import Yesod.Auth.OAuth2.Provider
import Yesod.Auth.OAuth2.UserId

oauth2BattleNet
    :: Text -- ^ Lower-case region (cn, us, etc)
    -> Provider m UserId
oauth2BattleNet region = Provider
    { pName = "battle.net"
    , pAuthorizeEndpoint = const $ AuthorizeEndpoint $ wwwPath "/oauth/authorize"
    , pAccessTokenEndpoint = AccessTokenEndpoint $ wwwPath "/oauth/token"
    , pFetchUserProfile = authGetProfile $ apiPath "/account/user"
    }
  where
    apiPath = fromRelative "https" (apiHost region)
    wwwPath = fromRelative "https" (wwwHost region)

apiHost :: Text -> Host
apiHost "cn" = "api.battlenet.com.cn"
apiHost region = Host $ encodeUtf8 $ region <> ".api.battle.net"

wwwHost :: Text -> Host
wwwHost "cn" = "www.battlenet.com.cn"
wwwHost region = Host $ encodeUtf8 $ region <> ".battle.net"
