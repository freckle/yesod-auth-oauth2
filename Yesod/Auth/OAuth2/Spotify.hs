{-# LANGUAGE OverloadedStrings #-}

module Yesod.Auth.OAuth2.Spotify
    ( oauth2Spotify
    , module Yesod.Auth.OAuth2
    ) where

import Control.Applicative ((<$>), (<*>))
import Control.Exception.Lifted
import Control.Monad (mzero)
import Data.Aeson
import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Network.HTTP.Conduit(Manager)
import Yesod.Auth
import Yesod.Auth.OAuth2

data SpotifyUser = SpotifyUser
    { spotifyUserId          :: Text
    , spotifyUserHref        :: Text
    , spotifyUserDisplayName :: Text
    , spotifyProduct         :: Text
    }

instance FromJSON SpotifyUser where
    parseJSON (Object v) = SpotifyUser <$>
                           v .: "id" <*>
                           v .: "href" <*>
                           v .: "display_name" <*>
                           v .: "product"
    parseJSON _ = mzero

oauth2Spotify :: YesodAuth m
             => Text -- ^ Client ID
             -> Text -- ^ Client Secret
             -> AuthPlugin m
oauth2Spotify clientId clientSecret = authOAuth2 "spotify"
    (OAuth2
        { oauthClientId            = encodeUtf8 clientId
        , oauthClientSecret        = encodeUtf8 clientSecret
        , oauthOAuthorizeEndpoint  = "https://accounts.spotify.com/authorize"
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
    [ ("href"        , spotifyUserHref user)
    , ("display_name", spotifyUserDisplayName user)
    , ("product"     , spotifyProduct user)
    ]
