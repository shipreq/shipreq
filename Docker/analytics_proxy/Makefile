.PHONY: default build run help exec up down test

IMAGE          = shipreq/analytics_proxy
CONTAINER      = analytics_proxy
STATCOUNTER_JS = www.statcounter.com/counter/counter.js

DOCKER_ARGS  = --name $(CONTAINER)
DOCKER_ARGS += --rm
DOCKER_ARGS += -p3000:80

default: run

build: Dockerfile package.json package-lock.json src LICENSE config.js
	docker build -t $(IMAGE) .

run: build
	docker run $(DOCKER_ARGS) $(IMAGE)

exec: build
	docker run $(DOCKER_ARGS) -it $(IMAGE) bash

up: build
	-@docker rm -f $(CONTAINER) &>/dev/null
	docker run $(DOCKER_ARGS) -d $(IMAGE)

down:
	docker rm -f $(CONTAINER)

test:
	APP__ENV_NAME=test NODE_ENV=test PORT=3000 npm start

node_modules:
	npm install

sc_mask: node_modules
	npm run mask $(STATCOUNTER_JS)
	@echo =====================================================================================
	npm run mask c.statcounter.com

dev-js/node_modules:
	cd dev-js && yarn

sc_download: dev-js/node_modules
	curl $(STATCOUNTER_JS) -o statcounter.min.js
	./dev-js/node_modules/.bin/js-beautify -s 2 statcounter.min.js > statcounter.js
	git add statcounter.min.js statcounter.js

sc_test:
	curl -s http://localhost:3000/$(STATCOUNTER_JS) | ./dev-js/node_modules/.bin/js-beautify -s 2 | bat -l js -f - | less -SR
