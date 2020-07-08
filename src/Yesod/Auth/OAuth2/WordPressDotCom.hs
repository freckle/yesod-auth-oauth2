{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.WordPressDotCom where

import qualified Data.Text as T
import Yesod.Auth.OAuth2.Prelude

pluginName :: Text
pluginName = "WordPress.com"

newtype WpUser = WpUser Int

instance FromJSON WpUser where
    parseJSON = withObject "WpUser" $ \o -> WpUser
        <$> o .: "ID"

oauth2WordPressDotCom
    :: ( YesodAuth m )
    => Text -- client ID
    -> Text -- client secret
    -> AuthPlugin m
oauth2WordPressDotCom clientId clientSecret =
    authOAuth2 pluginName oauth2 $ \manager token -> do
        (WpUser userId, userResponse) <-
            authGetProfile pluginName manager token
                "https://public-api.wordpress.com/rest/v1/me/"

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = T.pack $ show userId
            , credsExtra = setExtra token userResponse
            }

  where
    oauth2 = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint =
            "https://public-api.wordpress.com/oauth2/authorize"
                `withQuery` [ scopeParam "," [ "auth" ] ]
        , oauthAccessTokenEndpoint =
            "https://public-api.wordpress.com/oauth2/token"
        , oauthCallback = Nothing
        }
