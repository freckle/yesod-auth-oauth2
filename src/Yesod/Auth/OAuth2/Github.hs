module Yesod.Auth.OAuth2.Github
    {-# DEPRECATED "Please use Yesod.Auth.OAuth2.GitHub (GitHub, not Github)" #-}
    ( oauth2Github
    , oauth2GithubScoped
    ) where


import Yesod.Auth.OAuth2.GitHub
import Yesod.Auth.OAuth2.Prelude

oauth2Github :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2Github = oauth2GitHub

oauth2GithubScoped :: YesodAuth m => [Text] -> Text -> Text -> AuthPlugin m
oauth2GithubScoped = oauth2GitHubScoped
