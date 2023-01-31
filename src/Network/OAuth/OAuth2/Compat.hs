{-# LANGUAGE CPP #-}

module Network.OAuth.OAuth2.Compat
    ( OAuth2(..)
    , OAuth2Result
    , Error
    , authorizationUrl
    , fetchAccessToken
    , fetchAccessToken2
    , authGetBS

    -- * Re-exports
    , module Network.OAuth.OAuth2
    ) where

import Data.ByteString.Lazy (ByteString)
import Data.Text (Text)
import Network.HTTP.Conduit (Manager)
#if MIN_VERSION_hoauth2(2,7,0)
import Network.OAuth.OAuth2
    ( AccessToken(..)
    , ExchangeToken(..)
    , OAuth2Token(..)
    , RefreshToken(..)
    )
#else
import Network.OAuth.OAuth2
    ( AccessToken(..)
    , ExchangeToken(..)
    , OAuth2Error
    , OAuth2Token(..)
    , RefreshToken(..)
    )
#endif
import qualified Network.OAuth.OAuth2 as OAuth2
#if MIN_VERSION_hoauth2(2,7,0)
import Network.OAuth.OAuth2.TokenRequest (TokenRequestError)
#else
import Network.OAuth.OAuth2.TokenRequest (Errors)
#endif
import URI.ByteString

#if MIN_VERSION_hoauth2(2,2,0)
import Control.Monad.Trans.Except (ExceptT, runExceptT)
import Data.Maybe (fromMaybe)
#endif

data OAuth2 = OAuth2
    { oauth2ClientId :: Text
    , oauth2ClientSecret :: Maybe Text
    , oauth2AuthorizeEndpoint :: URIRef Absolute
    , oauth2TokenEndpoint :: URIRef Absolute
    , oauth2RedirectUri :: Maybe (URIRef Absolute)
    }

#if MIN_VERSION_hoauth2(2,7,0)
type Error = TokenRequestError
#else
type Error = OAuth2Error Errors
#endif

type OAuth2Result err a = Either err a

authorizationUrl :: OAuth2 -> URI
authorizationUrl = OAuth2.authorizationUrl . getOAuth2

fetchAccessToken
    :: Manager
    -> OAuth2
    -> ExchangeToken
    -> IO (OAuth2Result Error OAuth2Token)
fetchAccessToken = fetchAccessTokenBasic

fetchAccessToken2
    :: Manager
    -> OAuth2
    -> ExchangeToken
    -> IO (OAuth2Result Error OAuth2Token)
fetchAccessToken2 = fetchAccessTokenPost

authGetBS :: Manager -> AccessToken -> URI -> IO (Either ByteString ByteString)
authGetBS m a u = runOAuth2 $ OAuth2.authGetBS m a u

-- Normalize the rename of record fields at hoauth2-2.0. Our type is the newer
-- names and we up-convert if hoauth2-1.x is in use. getClientSecret and
-- getRedirectUri handle the differences in hoauth2-2.2 and 2.3.

#if MIN_VERSION_hoauth2(2,0,0)
getOAuth2 :: OAuth2 -> OAuth2.OAuth2
getOAuth2 o = OAuth2.OAuth2
    { OAuth2.oauth2ClientId = oauth2ClientId o
    , OAuth2.oauth2ClientSecret = getClientSecret $ oauth2ClientSecret o
    , OAuth2.oauth2AuthorizeEndpoint = oauth2AuthorizeEndpoint o
    , OAuth2.oauth2TokenEndpoint = oauth2TokenEndpoint o
    , OAuth2.oauth2RedirectUri = getRedirectUri $ oauth2RedirectUri o
    }
#else
getOAuth2 :: OAuth2 -> OAuth2.OAuth2
getOAuth2 o = OAuth2.OAuth2
    { OAuth2.oauthClientId = oauth2ClientId o
    , OAuth2.oauthClientSecret = getClientSecret $ oauth2ClientSecret o
    , OAuth2.oauthOAuthorizeEndpoint = oauth2AuthorizeEndpoint o
    , OAuth2.oauthAccessTokenEndpoint = oauth2TokenEndpoint o
    , OAuth2.oauthCallback = getRedirectUri $ oauth2RedirectUri o
    }
#endif

-- hoauth2-2.2 made oauth2ClientSecret non-Maybe, after 2.0 had just made it
-- Maybe so we have to adjust, twice. TODO: change ours type to non-Maybe (major
-- bump) and reverse this to up-convert with Just in pre-2.2.

#if MIN_VERSION_hoauth2(2,2,0)
getClientSecret :: Maybe Text -> Text
getClientSecret =
    fromMaybe $ error "Cannot use OAuth2.oauth2ClientSecret with Nothing"
#else
getClientSecret :: Maybe Text -> Maybe Text
getClientSecret = id
#endif

-- hoauth2-2.3 then made oauth2RedirectUri non-Maybe too. We logically rely on
-- instantiating with Nothing at definition-time, then setting it to the
-- callback at use-time, which means we can't just change our type and invert
-- this shim; we'll have to do something much more pervasive to avoid this
-- fromMaybe.

#if MIN_VERSION_hoauth2(2,3,0)
getRedirectUri :: Maybe (URIRef Absolute) -> (URIRef Absolute)
getRedirectUri =
    fromMaybe $ error "Cannot use OAuth2.oauth2RedirectUri with Nothing"
#else
getRedirectUri :: Maybe (URIRef Absolute) -> Maybe (URIRef Absolute)
getRedirectUri = id
#endif

-- hoauth-2.2 moved most IO-Either functions to ExceptT. This reverses that.

#if MIN_VERSION_hoauth2(2,2,0)
runOAuth2 :: ExceptT e m a -> m (Either e a)
runOAuth2 = runExceptT
#else
runOAuth2 :: IO (Either e a) -> IO (Either e a)
runOAuth2 = id
#endif

-- The fetchAccessToken functions grew a nicer interface in hoauth2-2.3. This
-- up-converts the older ones. We should update our code to use these functions
-- directly.

fetchAccessTokenBasic
    :: Manager
    -> OAuth2
    -> ExchangeToken
    -> IO (OAuth2Result Error OAuth2Token)
fetchAccessTokenBasic m o e = runOAuth2 $ f m (getOAuth2 o) e
  where
#if MIN_VERSION_hoauth2(2,6,0)
    f = OAuth2.fetchAccessTokenWithAuthMethod OAuth2.ClientSecretBasic
#elif MIN_VERSION_hoauth2(2,3,0)
    f = OAuth2.fetchAccessTokenInternal OAuth2.ClientSecretBasic
#else
    f = OAuth2.fetchAccessToken
#endif

fetchAccessTokenPost
    :: Manager
    -> OAuth2
    -> ExchangeToken
    -> IO (OAuth2Result Error OAuth2Token)
fetchAccessTokenPost m o e = runOAuth2 $ f m (getOAuth2 o) e
  where
#if MIN_VERSION_hoauth2(2, 6, 0)
    f = OAuth2.fetchAccessTokenWithAuthMethod OAuth2.ClientSecretPost
#elif MIN_VERSION_hoauth2(2,3,0)
    f = OAuth2.fetchAccessTokenInternal OAuth2.ClientSecretPost
#else
    f = OAuth2.fetchAccessToken2
#endif
