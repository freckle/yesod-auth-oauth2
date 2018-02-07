all: setup build test lint

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests
	stack install $(STACK_ARGUMENTS) hlint weeder

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) \
	  --flag yesod-auth-oauth2:example \
	  --pedantic --test --no-run-tests

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) --test


.PHONY: lint
lint:
	hlint src test
	weeder .
