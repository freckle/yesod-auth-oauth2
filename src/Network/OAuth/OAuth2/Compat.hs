{-# LANGUAGE CPP #-}

module Network.OAuth.OAuth2.Compat
  ( OAuth2 (..)
  , authorizationUrl
  , fetchAccessTokenBasic
  , fetchAccessTokenPost
  , authGetBS

    -- * Re-exports
  , AccessToken (..)
  , ExchangeToken (..)
  , RefreshToken (..)
  , TokenResponse
  , accessToken
  , refreshToken
  , expiresIn
  , tokenType
  , idToken
  , TokenResponseError
  ) where

import Control.Monad.IO.Class (MonadIO)
import Control.Monad.Trans.Except (runExceptT)
import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Conduit (Manager)
import URI.ByteString

#if MIN_VERSION_hoauth2(2,15,0)
import Network.OAuth2
  ( AccessToken (..)
  , ExchangeToken (..)
  , RefreshToken (..)
  , TokenResponse (..)
  , TokenResponseError
  )
import qualified Network.OAuth2 as OAuth2

#elif MIN_VERSION_hoauth2(2,9,0)
import Network.OAuth.OAuth2
  ( AccessToken (..)
  , ExchangeToken (..)
  , RefreshToken (..)
  , OAuth2Token (..)
  , TokenResponseError
  )
import qualified Network.OAuth.OAuth2 as OAuth2

type TokenResponse = OAuth2Token

#else
-- hoauth2-2.8
import Network.OAuth.OAuth2
  ( AccessToken (..)
  , ExchangeToken (..)
  , RefreshToken (..)
  , OAuth2Token (..)
  )
import Network.OAuth.OAuth2.TokenRequest (TokenRequestError)
import qualified Network.OAuth.OAuth2 as OAuth2

type TokenResponse = OAuth2Token
type TokenResponseError = TokenRequestError
#endif

data OAuth2 = OAuth2
  { oauth2ClientId :: Text
  , oauth2ClientSecret :: Text
  , oauth2AuthorizeEndpoint :: URIRef Absolute
  , oauth2TokenEndpoint :: URIRef Absolute
  , oauth2RedirectUri :: Maybe (URIRef Absolute)
  }

authorizationUrl :: OAuth2 -> URI
authorizationUrl = OAuth2.authorizationUrl . getOAuth2

fetchAccessTokenBasic
  :: Manager
  -> OAuth2
  -> ExchangeToken
  -> IO (Either TokenResponseError TokenResponse)
fetchAccessTokenBasic =
  runFetchAccessToken OAuth2.ClientSecretBasic

fetchAccessTokenPost
  :: Manager
  -> OAuth2
  -> ExchangeToken
  -> IO (Either TokenResponseError TokenResponse)
fetchAccessTokenPost =
  runFetchAccessToken OAuth2.ClientSecretPost

authGetBS :: Manager -> AccessToken -> URI -> IO (Either ByteString ByteString)
authGetBS m a u = runExceptT $ OAuth2.authGetBS m a u

getOAuth2 :: OAuth2 -> OAuth2.OAuth2
getOAuth2 o =
  OAuth2.OAuth2
    { OAuth2.oauth2ClientId = oauth2ClientId o
    , OAuth2.oauth2ClientSecret = oauth2ClientSecret o
    , OAuth2.oauth2AuthorizeEndpoint = oauth2AuthorizeEndpoint o
    , OAuth2.oauth2TokenEndpoint = oauth2TokenEndpoint o
    , OAuth2.oauth2RedirectUri = case oauth2RedirectUri o of
        Nothing ->
          error
            "programmer error: yesod-auth-oauth2:OAuth2 must have a Just value set as oauth2RedirectUri before using as an hauth2:OAuth2 value"
        Just uri -> uri
    }

runFetchAccessToken
  :: MonadIO m
  => OAuth2.ClientAuthenticationMethod
  -> Manager
  -> OAuth2
  -> ExchangeToken
  -> m (Either TokenResponseError TokenResponse)
runFetchAccessToken am m o e = runExceptT $ OAuth2.fetchAccessTokenWithAuthMethod am m (getOAuth2 o) e
