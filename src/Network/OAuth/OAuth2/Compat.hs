{-# LANGUAGE CPP #-}

module Network.OAuth.OAuth2.Compat
    ( OAuth2(..)
    , authorizationUrl
    , fetchAccessToken
    , fetchAccessToken2
    , module Network.OAuth.OAuth2
    ) where

import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2 hiding
    (OAuth2(..), authorizationUrl, fetchAccessToken, fetchAccessToken2)
import qualified Network.OAuth.OAuth2 as OAuth2
import Network.OAuth.OAuth2.TokenRequest (Errors)
import URI.ByteString

#if MIN_VERSION_hoauth2(2,0,0)
import Network.OAuth.OAuth2 (OAuth2(..))

getOAuth2 :: OAuth2 -> OAuth2
getOAuth2 = id

#else
import Data.Text (Text)

data OAuth2 = OAuth2
    { oauth2ClientId :: Text
    , oauth2ClientSecret :: Maybe Text
    , oauth2AuthorizeEndpoint :: URIRef Absolute
    , oauth2TokenEndpoint :: URIRef Absolute
    , oauth2RedirectUri :: Maybe (URIRef Absolute)
    }

getOAuth2 :: OAuth2 -> OAuth2.OAuth2
getOAuth2 o = OAuth2.OAuth2
    { OAuth2.oauthClientId = oauth2ClientId o
    , OAuth2.oauthClientSecret = oauth2ClientSecret o
    , OAuth2.oauthOAuthorizeEndpoint = oauth2AuthorizeEndpoint o
    , OAuth2.oauthAccessTokenEndpoint = oauth2TokenEndpoint o
    , OAuth2.oauthCallback = oauth2RedirectUri o
    }

#endif

authorizationUrl :: OAuth2 -> URI
authorizationUrl = OAuth2.authorizationUrl . getOAuth2

fetchAccessToken
    :: Manager
    -> OAuth2
    -> ExchangeToken
    -> IO (OAuth2Result Errors OAuth2Token)
fetchAccessToken m = OAuth2.fetchAccessToken m . getOAuth2

fetchAccessToken2
    :: Manager
    -> OAuth2
    -> ExchangeToken
    -> IO (OAuth2Result Errors OAuth2Token)
fetchAccessToken2 m = OAuth2.fetchAccessToken2 m . getOAuth2
