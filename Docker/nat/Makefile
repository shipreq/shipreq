.PHONY: default build run exec extract .extract test .test extract_and_test watch cert

SSL = ssl

INPUTS  = Dockerfile docker-entrypoint.sh squid.conf domain-whitelist.txt
INPUTS += $(SSL)/squid.crt $(SSL)/squid.csr $(SSL)/squid.key $(SSL)/squid.pem

IMAGE = shipreq/nat

CONTAINER = nat

DOCKER_ARGS  = --name $(CONTAINER)
DOCKER_ARGS += --rm
DOCKER_ARGS += --net=host

default: extract

$(SSL)/squid.key:
	-@mkdir -p $(SSL)
	cd $(SSL) && openssl genrsa -out squid.key 4096
	chmod 400 $@

$(SSL)/squid.csr: $(SSL)/squid.key
	cd $(SSL) && openssl req -new -key squid.key -out squid.csr -subj "/C=XX/ST=XX/L=squid/O=squid/CN=squid"
	chmod 400 $@

$(SSL)/squid.crt: $(SSL)/squid.csr
	cd $(SSL) && openssl x509 -req -days 3650 -in squid.csr -signkey squid.key -out squid.crt
	chmod 400 $@

$(SSL)/squid.pem: $(SSL)/squid.crt
	cd $(SSL) && cat squid.key squid.crt >> squid.pem
	chmod 400 $@

$(SSL): $(SSL)/squid.pem

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
	-./test-proxy
	-@echo "========================================================================================================================"
	-@docker logs $(CONTAINER) 2>&1 | sed '0,/.*\(twitter\|amazon\|mailchimp\|google\).*/d; /^[[:space:]]*$$/d' | tee test.log | head -60
	-@echo "========================================================================================================================"

extract_and_test: | up .extract .test down

watch:
	ls -1 Makefile $(INPUTS) ./test | entr -rc make extract_and_test
