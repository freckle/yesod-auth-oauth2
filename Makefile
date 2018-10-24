all: setup setup.lint dependencies build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool hlint weeder

.PHONY: dependencies
dependencies:
	stack build $(STACK_ARGUMENTS) -j 1 haskell-src-exts
	stack build $(STACK_ARGUMENTS) \
	  --flag yesod-auth-oauth2:example \
	  --dependencies-only --test --no-run-tests

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) \
	  --flag yesod-auth-oauth2:example \
	  --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) \
	  --flag yesod-auth-oauth2:example \
	  --pedantic --test


.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint src test
	stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: clean
clean:
	stack clean
