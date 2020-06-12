.PHONY: default build run exec extract .extract test .test extract_and_test watch

INPUTS = Dockerfile docker-entrypoint.sh squid.conf domain-whitelist.txt

IMAGE = shipreq/nat

CONTAINER = nat

DOCKER_ARGS = \
  --name $(CONTAINER) \
  --rm \
  --net=host

  # -p 3128:3128 \
  # -p 3129:3129 \
  # -p 3130:3130

default: extract

build: $(INPUTS)
	docker build -t $(IMAGE) .

run: build
	docker run $(DOCKER_ARGS) $(IMAGE)

exec: build
	docker run $(DOCKER_ARGS) -it $(IMAGE) /bin/sh

up: build
	-@docker rm -f $(CONTAINER) &>/dev/null
	docker run $(DOCKER_ARGS) -d $(IMAGE)
	sleep 0.8

down:
	docker rm -f $(CONTAINER)

extract: | up .extract down
.extract:
	docker cp $(CONTAINER):/etc/squid/squid.conf.orig squid-defaults.conf

test: | up .test down
.test:
	-./test run
	-@echo "========================================================================================================================"
	-@docker logs $(CONTAINER) 2>&1 | sed '0,/.*\(twitter\|amazon\|mailchimp\|google\).*/d; /^[[:space:]]*$$/d' | tee test.log | head -60
	-@echo "========================================================================================================================"

extract_and_test: | up .extract .test down

watch:
	ls -1 Makefile $(INPUTS) ./test | entr -rc make extract_and_test
