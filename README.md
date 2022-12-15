# Yesod.Auth.OAuth2


[![Hackage](https://img.shields.io/hackage/v/yesod-auth-oauth2.svg?style=flat)](https://hackage.haskell.org/package/yesod-auth-oauth2)
[![Stackage Nightly](http://stackage.org/package/yesod-auth-oauth2/badge/nightly)](http://stackage.org/nightly/package/yesod-auth-oauth2)
[![Stackage LTS](http://stackage.org/package/yesod-auth-oauth2/badge/lts)](http://stackage.org/lts/package/yesod-auth-oauth2)
[![CI](https://github.com/freckle/yesod-auth-oauth2/actions/workflows/ci.yml/badge.svg)](https://github.com/pbrisbin/freckle/yesod-auth-oauth2/workflows/ci.yml)

OAuth2 `AuthPlugin`s for Yesod.

## Usage

```hs
import Yesod.Auth
import Yesod.Auth.OAuth2.GitHub

instance YesodAuth App where
    -- ...

    authPlugins _ = [oauth2GitHub clientId clientSecret]

clientId :: Text
clientId = "..."

clientSecret :: Text
clientSecret = "..."
```

Some plugins, such as GitHub and Slack, have scoped functions for requesting
additional information:

```hs
oauth2SlackScoped [SlackBasicScope, SlackEmailScope] clientId clientSecret
```

## Working with Extra Data

We put the minimal amount of user data possible in `credsExtra` -- just enough
to support you parsing or fetching additional data yourself.

For example, if you work with GitHub and GitHub user profiles, you likely
already have a model and a way to parse the `/user` response. Rather than
duplicate all that in our library, we try to make it easy for you to re-use that
code yourself:

```hs
authenticate creds = do
    let
        -- You can run your own FromJSON parser on the response we already have
        eGitHubUser :: Either String GitHubUser
        eGitHubUser = getUserResponseJSON creds

        -- Avert your eyes, simplified example
        Just accessToken = getAccessToken creds
        Right githubUser = eGitHubUser

    -- Or make followup requests using our access token
    runGitHub accessToken $ userRepositories githubUser

    -- Or store it for later
    insert User
        { userIdent = credsIdent creds
        , userAccessToken = accessToken
        }
```

**NOTE**: Avoid looking up values in `credsExtra` yourself; prefer the provided
`get` functions. The data representation itself is no longer considered public
API.

## Local Providers

If we don't supply a "Provider" (e.g. GitHub, Google, etc) you need, you can
write your own using our provided `Prelude`:

```haskell
import Yesod.Auth.OAuth2.Prelude

pluginName :: Text
pluginName = "mysite"

oauth2MySite :: YesodAuth m => Text -> Text -> AuthPlugin m
oauth2MySite clientId clientSecret =
    authOAuth2 pluginName oauth2 $ \manager token -> do
        -- Fetch a profile using the manager and token, leave it a ByteString
        userResponse <- -- ...

        -- Parse it to your preferred identifier, e.g. with Data.Aeson
        userId <- -- ...

        -- See authGetProfile for the typical case

        pure Creds
            { credsPlugin = pluginName
            , credsIdent = userId
            , credsExtra = setExtra token userResponse
            }
  where
    oauth2 = OAuth2
        { oauth2ClientId = clientId
        , oauth2ClientSecret = Just clientSecret
        , oauth2AuthorizeEndpoint = "https://mysite.com/oauth/authorize"
        , oauth2TokenEndpoint = "https://mysite.com/oauth/token"
        , oauth2RedirectUri = Nothing
        }
```

The `Prelude` module is considered public API, though we may build something
higher-level that is more convenient for this use-case in the future.

## Development & Tests

```console
stack setup
stack build --dependencies-only
stack build --pedantic --test
```

Please also run HLint and Weeder before submitting PRs.

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
