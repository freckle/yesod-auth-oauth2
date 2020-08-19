all: setup setup.lint dependencies build test lint

.PHONY: setup
setup:
	stack setup

.PHONY: setup.lint
setup.lint:
	stack install --copy-compiler-tool hlint weeder

.PHONY: dependencies
dependencies:
	stack build \
	  --flag yesod-auth-oauth2:example \
	  --dependencies-only --test --no-run-tests

.PHONY: build
build:
	stack build \
	  --flag yesod-auth-oauth2:example \
	  --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build \
	  --flag yesod-auth-oauth2:example \
	  --pedantic --test


.PHONY: lint
lint:
	stack exec hlint src test
	stack exec weeder .

.PHONY: clean
clean:
	stack clean
