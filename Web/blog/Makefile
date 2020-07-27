GATSBY := ./node_modules/gatsby-cli/cli.js

.PHONY: clean cd cp dev prod dev-build prod-build watch-prod

default: dev

clean:
	$(GATSBY) clean

dev:
	$(GATSBY) develop

dev-build:
	$(GATSBY) build

prod-build:
	ENV=prod $(GATSBY) build
	find public -name '*.map' -delete

prod: prod-build
	$(GATSBY) serve

cd: clean dev
cp: clean prod

watch-prod: clean
	git ls-files | entr -cr make prod-build
