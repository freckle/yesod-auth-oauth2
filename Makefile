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

.PHONY: nightly
nightly:
	stack setup --stack-yaml stack-nightly.yaml --resolver nightly
	stack build --stack-yaml stack-nightly.yaml --resolver nightly \
	  --test --no-run-tests --bench --no-run-benchmarks \
	  --dependencies-only
	stack build --stack-yaml stack-nightly.yaml --resolver nightly \
	  --test --no-run-tests --bench --no-run-benchmarks \
	  --fast --pedantic

.PHONY: clean
clean:
	stack clean
