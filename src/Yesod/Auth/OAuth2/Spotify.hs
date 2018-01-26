{-# LANGUAGE OverloadedStrings #-}
-- |
--
-- OAuth2 plugin for http://spotify.com
--
module Yesod.Auth.OAuth2.Spotify
    ( oauth2Spotify
    ) where

import Yesod.Auth.OAuth2.Prelude

import Data.Maybe
import qualified Data.Text as T

data SpotifyUserImage = SpotifyUserImage
    { spotifyUserImageHeight :: Maybe Int
    , spotifyUserImageWidth :: Maybe Int
    , spotifyUserImageUrl :: Text
    }

instance FromJSON SpotifyUserImage where
    parseJSON = withObject "SpotifyUserImage" $ \v -> SpotifyUserImage
        <$> v .:? "height"
        <*> v .:? "width"
        <*> v .: "url"

data SpotifyUser = SpotifyUser
    { spotifyUserId :: Text
    , spotifyUserHref :: Text
    , spotifyUserUri :: Text
    , spotifyUserDisplayName :: Maybe Text
    , spotifyUserProduct :: Maybe Text
    , spotifyUserCountry :: Maybe Text
    , spotifyUserEmail :: Maybe Text
    , spotifyUserImages :: Maybe [SpotifyUserImage]
    }

instance FromJSON SpotifyUser where
    parseJSON = withObject "SpotifyUser" $ \v -> SpotifyUser
        <$> v .: "id"
        <*> v .: "href"
        <*> v .: "uri"
        <*> v .:? "display_name"
        <*> v .:? "product"
        <*> v .:? "country"
        <*> v .:? "email"
        <*> v .:? "images"

oauth2Spotify :: YesodAuth m
              => Text -- ^ Client ID
              -> Text -- ^ Client Secret
              -> [Text] -- ^ Scopes
              -> AuthPlugin m
oauth2Spotify clientId clientSecret scope = authOAuth2 "spotify"
    OAuth2
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://accounts.spotify.com/authorize" `withQuery`
            [ ("scope", encodeUtf8 $ T.intercalate " " scope)
            ]
        , oauthAccessTokenEndpoint = "https://accounts.spotify.com/api/token"
        , oauthCallback = Nothing
        }
    $ fromProfileURL "spotify" "https://api.spotify.com/v1/me" toCreds

toCreds :: SpotifyUser -> Creds m
toCreds user = Creds
    { credsPlugin = "spotify"
    , credsIdent = spotifyUserId user
    , credsExtra = mapMaybe getExtra extrasTemplate
    }

  where
    userImage :: Maybe SpotifyUserImage
    userImage = spotifyUserImages user >>= listToMaybe

    userImagePart :: (SpotifyUserImage -> Maybe a) -> Maybe a
    userImagePart getter = userImage >>= getter

    extrasTemplate = [ ("href", Just $ spotifyUserHref user)
                     , ("uri", Just $ spotifyUserUri user)
                     , ("display_name", spotifyUserDisplayName user)
                     , ("product", spotifyUserProduct user)
                     , ("country", spotifyUserCountry user)
                     , ("email", spotifyUserEmail user)
                     , ("image_url", spotifyUserImageUrl <$> userImage)
                     , ("image_height", T.pack . show <$> userImagePart spotifyUserImageHeight)
                     , ("image_width", T.pack . show <$> userImagePart spotifyUserImageWidth)
                     ]

    getExtra :: (Text, Maybe Text) -> Maybe (Text, Text)
    getExtra (key, val) = fmap ((,) key) val
