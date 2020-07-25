GATSBY := ./node_modules/gatsby-cli/cli.js

.PHONY: clean cd dev prod dev-build prod-build

default: dev

clean:
	$(GATSBY) clean

dev:
	$(GATSBY) develop

cd: clean dev

dev-build:
	$(GATSBY) build

prod-build:
	ENV=prod $(GATSBY) build

prod: prod-build
	$(GATSBY) serve
