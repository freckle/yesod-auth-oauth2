{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.Spotify
    ( oauth2Spotify
    , module Yesod.Auth.OAuth2
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.ByteString (ByteString)
import Data.Maybe
import Data.Text (Text)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit(Manager)
import Yesod.Auth
import Yesod.Auth.OAuth2
import qualified Data.ByteString as B
import qualified Data.Text as T

data SpotifyUserImage = SpotifyUserImage
    { spotifyUserImageHeight :: Maybe Int
    , spotifyUserImageWidth :: Maybe Int
    , spotifyUserImageUrl :: Text
    }

instance FromJSON SpotifyUserImage where
    parseJSON (Object v) = SpotifyUserImage <$>
                           v .: "height" <*>
                           v .: "width" <*>
                           v .: "url"

    parseJSON _ = mzero

data SpotifyUser = SpotifyUser
    { spotifyUserId          :: Text
    , spotifyUserHref        :: Text
    , spotifyUserUri         :: Text
    , spotifyUserDisplayName :: Maybe Text
    , spotifyUserProduct     :: Maybe Text
    , spotifyUserCountry     :: Maybe Text
    , spotifyUserEmail       :: Maybe Text
    , spotifyUserImages      :: Maybe [SpotifyUserImage]
    }

instance FromJSON SpotifyUser where
    parseJSON (Object v) = SpotifyUser <$>
                           v .: "id" <*>
                           v .: "href" <*>
                           v .: "uri" <*>
                           v .:? "display_name" <*>
                           v .:? "product" <*>
                           v .:? "country" <*>
                           v .:? "email" <*>
                           v .:? "images"
    parseJSON _ = mzero

oauth2Spotify :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> [ByteString] -- ^ Scopes
             -> AuthPlugin m
oauth2Spotify clientId clientSecret scope = authOAuth2 "spotify"
    (OAuth2
        { oauthClientId            = encodeUtf8 clientId
        , oauthClientSecret        = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint  = B.append "https://accounts.spotify.com/authorize?scope=" (B.intercalate "%20" scope)
        , oauthAccessTokenEndpoint = "https://accounts.spotify.com/api/token"
        , oauthCallback            = Nothing
        })
    fetchSpotifyProfile

fetchSpotifyProfile :: Manager -> AccessToken -> IO (Creds m)
fetchSpotifyProfile manager token = do
    result <- authGetJSON manager token "https://api.spotify.com/v1/me"
    case result of
        Right user -> return $ toCreds user
        Left err -> throwIO $ InvalidProfileResponse "spotify" err

toCreds :: SpotifyUser -> Creds m
toCreds user = Creds "spotify"
    (spotifyUserId user)
    (mapMaybe getExtra extrasTemplate)

  where
    userImage :: Maybe SpotifyUserImage
    userImage = spotifyUserImages user >>= listToMaybe

    userImagePart :: (SpotifyUserImage -> Maybe a) -> Maybe a
    userImagePart getter = userImage >>= getter

    extrasTemplate = [ ("href"        , Just $ spotifyUserHref user)
                     , ("uri"         , Just $ spotifyUserUri user)
                     , ("display_name", spotifyUserDisplayName user)
                     , ("product"     , spotifyUserProduct user)
                     , ("country"     , spotifyUserCountry user)
                     , ("email"       , spotifyUserEmail user)
                     , ("image_url"   , userImage >>=
                                        return . spotifyUserImageUrl)
                     , ("image_height", userImagePart spotifyUserImageHeight >>=
                                        return . T.pack . show)
                     , ("image_width" , userImagePart spotifyUserImageWidth >>=
                                        return . T.pack . show)
                     ]

    getExtra :: (Text, Maybe Text) -> Maybe (Text, Text)
    getExtra (key, val) = fmap ((,) key) val
