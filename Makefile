all: setup setup.lint dependencies build test lint

.PHONY: setup
setup:
	stack setup

.PHONY: setup.lint
setup.lint:
	stack install --copy-compiler-tool hlint

.PHONY: setup.tools
setup.tools:
	stack install --copy-compiler-tool brittany stylish-haskell fast-tags

.PHONY: dependencies
dependencies:
	stack build \
	  --flag yesod-auth-oauth2:example \
	  --dependencies-only --test --no-run-tests

.PHONY: build
build:
	stack build \
	  --flag yesod-auth-oauth2:example \
	  --fast --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build \
	  --flag yesod-auth-oauth2:example \
	  --fast --pedantic --test

.PHONY: watch
watch:
	stack build \
	  --flag yesod-auth-oauth2:example \
	  --fast --pedantic --test --file-watch


.PHONY: lint
lint:
	stack exec hlint src test

.PHONY: nightly
nightly:
	stack setup --resolver nightly
	stack build --resolver nightly \
	  --test --no-run-tests --bench --no-run-benchmarks \
	  --dependencies-only
	stack build --resolver nightly \
	  --test --no-run-tests --bench --no-run-benchmarks \
	  --fast --pedantic

.PHONY: example
example: build
	stack exec yesod-auth-oauth2-example

.PHONY: clean
clean:
	stack clean
