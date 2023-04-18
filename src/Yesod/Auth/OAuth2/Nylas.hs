{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.Nylas
  ( oauth2Nylas
  ) where

import Yesod.Auth.OAuth2.Prelude

import Control.Monad (unless)
import qualified Data.ByteString.Lazy.Char8 as BL8
import Network.HTTP.Client
import qualified Network.HTTP.Types as HT
import qualified Yesod.Auth.OAuth2.Exception as YesodOAuth2Exception

newtype User = User Text

instance FromJSON User where
  parseJSON = withObject "User" $ \o -> User <$> o .: "id"

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
    let userResponse = responseBody resp

    -- FIXME: was this working? I'm 95% sure that the client will throw its
    -- own exception on unsuccessful status codes.
    unless (HT.statusIsSuccessful $ responseStatus resp)
      $ throwIO
      $ YesodOAuth2Exception.GenericError pluginName
      $ "Unsuccessful HTTP response: "
      <> BL8.unpack userResponse

    either
        (throwIO . YesodOAuth2Exception.JSONDecodingError pluginName)
        (\(User userId) -> pure Creds
          { credsPlugin = pluginName
          , credsIdent = userId
          , credsExtra = setExtra token userResponse
          }
        )
      $ eitherDecode userResponse
 where
  oauth = OAuth2
    { oauth2ClientId = clientId
    , oauth2ClientSecret = Just clientSecret
    , oauth2AuthorizeEndpoint =
      "https://api.nylas.com/oauth/authorize"
        `withQuery` [ ("response_type", "code")
                    , ("client_id", encodeUtf8 clientId)
          -- N.B. The scopes delimeter is unknown/untested. Verify that before
          -- extracting this to an argument and offering a Scoped function. In
          -- its current state, it doesn't matter because it's only one scope.
                    , scopeParam "," defaultScopes
                    ]
    , oauth2TokenEndpoint = "https://api.nylas.com/oauth/token"
    , oauth2RedirectUri = Nothing
    , oauth2AppRoot = Nothing
    }
