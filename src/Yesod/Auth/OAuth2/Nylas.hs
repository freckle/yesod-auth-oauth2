{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.Nylas
    ( oauth2Nylas
    ) where

import Yesod.Auth.OAuth2.Prelude

import Control.Monad (unless)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.HTTP.Client
import qualified Network.HTTP.Types as HT

newtype User = User Text

instance FromJSON User where
    parseJSON = withObject "User" $ \o -> User
        <$> o .: "id"

pluginName :: Text
pluginName = "nylas"

defaultScopes :: [Text]
defaultScopes = ["email"]

oauth2Nylas :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Nylas clientId clientSecret =
    authOAuth2 pluginName oauth $ \manager token -> do
        req <- applyBasicAuth (encodeUtf8 $ atoken $ accessToken token) ""
            <$> parseRequest "https://api.nylas.com/account"
        resp <- httpLbs req manager
        let userResponseJSON = responseBody resp

        -- FIXME: was this working? I'm 95% sure that the client will throw its
        -- own exception on unsuccessful status codes.
        unless (HT.statusIsSuccessful $ responseStatus resp)
            $ throwIO $ InvalidProfileResponse pluginName
            $ "Unsuccessful HTTP response: " <> userResponseJSON


        either
            (throwIO . InvalidProfileResponse pluginName . BL8.pack)
            (\(User userId) -> pure Creds
                { credsPlugin = pluginName
                , credsIdent = userId
                , credsExtra =
                    [ ("accessToken", atoken $ accessToken token)
                    , ("userResponseJSON", decodeUtf8 $ BL.toStrict userResponseJSON)
                    ]
                }
            )
            $ eitherDecode userResponseJSON
  where
    oauth = OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://api.nylas.com/oauth/authorize" `withQuery`
            [ ("response_type", "code")
            , ("client_id", encodeUtf8 clientId)
            -- N.B. The scopes delimeter is unknown/untested. Verify that before
            -- extracting this to an argument and offering a Scoped function. In
            -- its current state, it doesn't matter because it's only one scope.
            , scopeParam "," defaultScopes
            ]
        , oauthAccessTokenEndpoint = "https://api.nylas.com/oauth/token"
        , oauthCallback = Nothing
        }
