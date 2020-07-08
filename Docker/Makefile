.PHONY: info build-local publish

BRANCH := $(shell git rev-parse --abbrev-ref HEAD)

info:
	@echo "This Makefile is a collection of helpful utilities around Docker images used in the ShipReq ecosystem."
	@echo
	@echo "Commands:"
	@echo
	@echo "  * build-local -- Pull and/or build all Docker images locally."
	@echo "  * publish     -- Publish to AWS, start a build."
	@echo

build-local:
	$$(aws ecr get-login --no-include-email)
	PULL_REPO=930801536115.dkr.ecr.ap-southeast-2.amazonaws.com \
	ANALYTICS_PROXY_URL="shipreq/analytics_proxy" \
	DEV_BUILD_ENV_URL="shipreq/dev/build_env" \
	DEV_NODE_URL="shipreq/dev/node" \
	DEV_POSTGRES_URL="shipreq/dev/postgres" \
	NAT_URL="shipreq/nat" \
	OPS_CADVISOR_URL="shipreq/ops/cadvisor" \
	OPS_ECS_EXPORTER_URL="shipreq/ops/ecs_exporter" \
	OPS_FILEBEAT_URL="shipreq/ops/filebeat" \
	OPS_GRAFANA_URL="shipreq/ops/grafana" \
	OPS_NODE_EXPORTER_URL="shipreq/ops/node_exporter" \
	OPS_PORTAL_URL="shipreq/ops/portal" \
	OPS_POSTGRES_EXPORTER_URL="shipreq/ops/postgres_exporter" \
	OPS_PROMETHEUS_BIZ_URL="shipreq/ops/prometheus-biz" \
	OPS_PROMETHEUS_TECH_URL="shipreq/ops/prometheus-tech" \
	OPS_SQUID_EXPORTER_URL="shipreq/ops/squid_exporter" \
	SHIPREQ_BASE_URL="shipreq/base" \
	./build-all --no-push

publish:
ifneq ($(BRANCH),master)
	$(error You need to be on the master branch, not $(BRANCH))
else
	git push aws master
	aws codebuild start-build --project-name images
endif
