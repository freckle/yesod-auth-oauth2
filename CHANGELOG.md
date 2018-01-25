## [*Unreleased*](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.3.1...master)

- COMPATIBILITY: Support GHC-8.2
- COMPATIBILITY: Drop support for GHC-7.8

## [v0.3.1](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.3.0...v0.3.1)

- Internal project cleanup

## [v0.3.0](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.2.4...v0.3.0)

- COMPATIBILITY: Use `hoauth2-1.3`

## [v0.2.4](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.2.1...v0.2.4)

- FIX: Update Nylas provider
- NEW: Battle.Net provider
- NEW: Bitbucket provider
- NEW: Salesforce provider

## [v0.2.1](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.2.0...v0.2.1)

- FIX: Fix collision in GitHub `email` / `public_email` extras value

## [v0.2.0](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.10...v0.2.0)

- NEW: Slack provider ([@jsteiner](https://github.com/thoughtbot/yesod-auth-oauth2/commit/aad8bd88eabf9fcf368d044e7003e5d323985837))

## [v0.1.10](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.9...v0.1.10)

- FIX: `location` is optional in GitHub response

## [v0.1.9](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.8...v0.1.9)

- COMPATIBILITY: Allow `transformers-0.5` ([@paul-rouse](https://github.com/thoughtbot/yesod-auth-oauth2/commit/120104b5348808f72877962c329a998434addace))

## [v0.1.8](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.7...v0.1.8)

- COMPATIBILITY: Allow `aeson-0.11` ([@k-bx](https://github.com/thoughtbot/yesod-auth-oauth2/commit/6e940b19e2d56080c7a749aeb29e143a17dad65c))

## [v0.1.7](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.6...v0.1.7)

- NEW: Prefer primary email in GitHub provider
- NEW: Include `public_email` in GitHub extras response
- REMOVED: Remove Twitter provider

## [v0.1.6](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.5...v0.1.6)

- NEW: Nicer error message on invalid `code` ([@silky](https://github.com/thoughtbot/yesod-auth-oauth2/commit/7354c36e1326d298e543fa65cf226153ed4a8a0b))

## [v0.1.5](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.4...v0.1.5)

- FIX: Incorrect `state` parameter handling

## [v0.1.4](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.3...v0.1.4)

- FIX: Use newer Nylas endpoint

## [v0.1.3](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.2...v0.1.3)

- NEW: EveOnline provider ([@Drezil](https://github.com/thoughtbot/yesod-auth-oauth2/pull/33))
- NEW: Nylas provider ([@bts](https://github.com/thoughtbot/yesod-auth-oauth2/commit/815d44346403af0052a48aa844f506211bdc2863))

## [v0.1.2](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.1...v0.1.2)

- NEW: A more different Google provider ([@ssaavedra](https://github.com/thoughtbot/yesod-auth-oauth2/pull/32))

## [v0.1.1](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.1.0...v0.1.1)

- NEW: Twitter provider

## [v0.1.0](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.12...v0.1.0)

- REMOVED: Google provider, use `Yesod.Auth.GoogleEmail2`
- CHANGED: Learn was renamed to Upcase
- COMPATIBILITY: Drop support for GHC-6
- COMPATIBILITY: Support GHC-7.10

## [v0.0.12](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.11...v0.0.12)

- COMPATIBILITY: Allow `transformers-0.4` ([@snoyberg](https://github.com/thoughtbot/yesod-auth-oauth2/pull/21))

## [v0.0.11](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.10...v0.0.11)

- COMPATIBILITY: Allow `aeson-0.8` ([@gfontenot](https://github.com/thoughtbot/yesod-auth-oauth2/pull/15))

## [v0.0.10](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.9...v0.0.10)

- COMPATIBILITY: Allow Yesod 1.4 ([@gregwebs](https://github.com/thoughtbot/yesod-auth-oauth2/pull/14))

## [v0.0.9](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.8...v0.0.9)

- NEW: Spotify ([@benekastah](https://github.com/thoughtbot/yesod-auth-oauth2/pull/13))

## [v0.0.8](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.7...v0.0.8)

- FIX: Username may be missing in GitHub responses ([@skade](https://github.com/thoughtbot/yesod-auth-oauth2/pull/12))

## [v0.0.7](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.6...v0.0.7)

- NEW: Scope support in GitHub provider ([@skade](https://github.com/thoughtbot/yesod-auth-oauth2/pull/11))

## [v0.0.6](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.5.1...v0.0.6)

- NEW: GitHub provider ([@freiric](https://github.com/thoughtbot/yesod-auth-oauth2/pull/10))
- COMPATIBILITY: flag-driven `network`/`network-uri` dependency

## [v0.0.5.1](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.5...v0.0.5.1)

- DOCUMENTATION: fix data declaration, allows Haddocks to build

## [v0.0.5](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.4...v0.0.5)

- COMPATIBILITY: Allow `yesod-core-1.3` and target `yesod-auth-1.3` ([@maxcan](https://github.com/thoughtbot/yesod-auth-oauth2/pull/7))
- COMPATIBILITY: Target `haouth2-0.4` ([@katyo](https://github.com/thoughtbot/yesod-auth-oauth2/pull/9))

## [v0.0.4](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.3...v0.0.4)

- COMPATIBILITY: Allow `text-1.*`
- COMPATIBILITY: Allow `lifted-base-0.2.*`

## [v0.0.3](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.2...v0.0.3)

- FIX: replace `error` crash with `throwIO` exception

## [v0.0.2](https://github.com/thoughtbot/yesod-auth-oauth2/compare/v0.0.1...v0.0.2)

- Various documentation fixes.

## [v0.0.1](https://github.com/thoughtbot/yesod-auth-oauth2/tree/v0.0.1)

Initial version. Maintainer-ship taken over by
[@pbrisbin](https://github.com/thoughtbot/yesod-auth-oauth2/pull/1).
