.PHONY: default build run help exec up down test

IMAGE     = shipreq/analytics_proxy
CONTAINER = analytics_proxy

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

mask_sc: node_modules
	npm run mask 'www.statcounter.com/counter/counter.js'
