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
	stack exec weeder .

.PHONY: nightly
nightly:
	stack setup --stack-yaml stack-nightly.yaml --resolver nightly
	stack build --stack-yaml stack-nightly.yaml --resolver nightly \
	  --test --no-run-tests --bench --no-run-benchmarks \
	  --dependencies-only
	stack build --stack-yaml stack-nightly.yaml --resolver nightly \
	  --test --no-run-tests --bench --no-run-benchmarks \
	  --fast --pedantic

.PHONY: example
example: build
	stack exec yesod-auth-oauth2-example

.PHONY: clean
clean:
	stack clean
