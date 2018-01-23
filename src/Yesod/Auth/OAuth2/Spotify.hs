{-# LANGUAGE OverloadedStrings #-}
module Yesod.Auth.OAuth2.Spotify
    ( oauth2Spotify
    ) where

import Yesod.Auth.OAuth2.Provider
import Yesod.Auth.OAuth2.UserId

oauth2Spotify :: [Scope] -> Provider m UserIdText
oauth2Spotify scopes = Provider
    { pName = "spotify"
    , pAuthorizeEndpoint = const $ AuthorizeEndpoint
        $ "https://accounts.spotify.com/authorize" `withQuery`
            [ scopeParam " " scopes
            ]
    , pAccessTokenEndpoint = "https://accounts.spotify.com/api/token"
    , pFetchUserProfile = authGetProfile "https://api.spotify.com/v1/me"
    }
