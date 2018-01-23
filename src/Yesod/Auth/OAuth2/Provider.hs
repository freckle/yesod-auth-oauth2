{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
module Yesod.Auth.OAuth2.Provider
    ( ClientId(..)
    , ClientSecret(..)
    , AuthorizeEndpoint(..)
    , AccessTokenEndpoint(..)
    , ProviderName(..)
    , Provider(..)
    , authGetProfile
    , providerCreds
    , Scope(..)
    , scopeParam
    , withQuery
    , ToIdent(..)
    ) where

import Control.Monad.Trans.Except
import Data.Aeson (FromJSON, eitherDecode)
import Data.Bifunctor (first)
import qualified Data.ByteString as BS
import Data.ByteString.Lazy (ByteString, toStrict)
import Data.String (IsString)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Network.HTTP.Conduit (Manager)
import Network.OAuth.OAuth2
    (AccessToken(..), OAuth2Error(..), OAuth2Token(..), authGetBS)
import URI.ByteString (URI)
import URI.ByteString.Extension (withQuery)
import Yesod.Auth (Creds(..))

newtype ClientId = ClientId { clientId :: Text }
newtype ClientSecret = ClientSecret { clientSecret :: Text }

newtype AuthorizeEndpoint = AuthorizeEndpoint { authorizeEndpoint :: URI }
    deriving (IsString)

newtype AccessTokenEndpoint = AccessTokenEndpoint { accessTokenEndpoint :: URI }
    deriving (IsString)

newtype Scope = Scope { scope :: Text }
    deriving (IsString)

newtype ProviderName = ProviderName { providerName :: Text }
    deriving (IsString)

data Provider m a = Provider
    { pName :: ProviderName
    , pAuthorizeEndpoint :: ClientId -> AuthorizeEndpoint
    -- ^ Authorization endpoint
    --
    -- Some providers need to include the client-id in the request, so it's
    -- provided here. Most providers can ignore it
    --
    -- > pAuthorizeEndpoint = const "http://example.com/oauth2/authorize"
    --
    , pAccessTokenEndpoint :: AccessTokenEndpoint
    , pFetchUserProfile :: Manager -> AccessToken -> IO (Either Text ByteString)
    }

pParseUserProfile :: FromJSON a => Provider m a -> ByteString -> Either String a
pParseUserProfile _ = eitherDecode

authGetProfile :: URI -> Manager -> AccessToken -> IO (Either Text ByteString)
authGetProfile uri manager token =
    first prettyOAuth2Error <$> authGetBS manager token uri
  where
    prettyOAuth2Error :: OAuth2Error Text -> Text
    prettyOAuth2Error = T.pack . show -- FIXME

class ToIdent a where
    toIdent :: a -> Text

instance ToIdent Int where
    toIdent = T.pack . show

instance ToIdent Text where
    toIdent = id

providerCreds :: (FromJSON a, ToIdent a) => Provider m a -> Manager -> OAuth2Token -> IO (Either Text (Creds m))
providerCreds p@Provider{..} manager token = runExceptT $ do
    lbs <- ExceptT $ pFetchUserProfile manager $ accessToken token
    user <- withExceptT T.pack $ ExceptT $ return $ pParseUserProfile p lbs

    return Creds
        { credsPlugin = providerName pName
        , credsIdent = toIdent user
        , credsExtra =
            [ ("accessToken", atoken $ accessToken token)
            , ("userResponseBody", decodeUtf8 $ toStrict lbs)
            ]
        }

scopeParam :: Text -> [Scope] -> (BS.ByteString, BS.ByteString)
scopeParam d = ("scope",) . encodeUtf8 . T.intercalate d . map scope
