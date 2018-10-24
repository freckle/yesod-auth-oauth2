# Yesod.Auth.OAuth2

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
duplicate all that in our own library, we try to make it easy for you to re-use
that code yourself:

```hs
authenticate creds = do
    let
        -- You can run your own FromJSON parser on the respose we already have
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

If we don't supply a "Provider" (e.g. GitHub, Google, etc) you need. You can
write your own within your codebase:

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
        { oauthClientId = clientId
        , oauthClientSecret = clientSecret
        , oauthOAuthorizeEndpoint = "https://mysite.com/oauth/authorize"
        , oauthAccessTokenEndpoint = "https://mysite.com/oauth/token"
        , oauthCallback = Nothing
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
