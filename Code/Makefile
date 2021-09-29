.PHONY: build_env ci-local ci-shell info deploy diff_prod docker-push-dev prod_ver publish pull_images push rm_targets since_prod watch_codebuild

GIT_SHA  := $(shell git rev-parse HEAD)
BRANCH   := $(shell git rev-parse --abbrev-ref HEAD)
CHANGES  := $(shell git status --porcelain)
PROD_REV := $(shell fgrep shipreq ../Terraform/prod/versions.tf | perl -pe 's/.*git-(.+?)".*/$$1/')
ECR      := 930801536115.dkr.ecr.ap-southeast-2.amazonaws.com
AWS      := bin/aws

WATCH_CODEBUILD = watch -n 6 -c aws-codebuild-status -t 'Name:shipreq'

CI_LOCAL_DOCKER_CONTAINER = shipreq_ci
CI_LOCAL_DOCKER_ARGS  = --name $(CI_LOCAL_DOCKER_CONTAINER)
CI_LOCAL_DOCKER_ARGS += --rm
CI_LOCAL_DOCKER_ARGS += --privileged
CI_LOCAL_DOCKER_ARGS += --network host
CI_LOCAL_DOCKER_ARGS += -v /var/run/docker.sock:/var/run/docker.sock
CI_LOCAL_DOCKER_ARGS += -v "$(PWD)":/Code:ro
CI_LOCAL_DOCKER_ARGS += -v "$(HOME)/.cache/coursier":/root/.cache/coursier:ro
CI_LOCAL_DOCKER_ARGS += -v "$(HOME)/.aws":/root/.aws:ro
CI_LOCAL_DOCKER_ARGS += --mount type=tmpfs,destination=/tmp,tmpfs-size=8g
CI_LOCAL_DOCKER_ARGS += -e SHIPREQ_RELEASE_TARGET=/tmp/shipreq-ci-release.sbt
CI_LOCAL_DOCKER_ARGS += -e LOCAL_CI=1
CI_LOCAL_DOCKER_ARGS += -e GIT_SHA=$(GIT_SHA)
CI_LOCAL_DOCKER_ARGS += -e BASE_IMAGE_URL=$(ECR)/shipreq/base
CI_LOCAL_DOCKER_ARGS += -e WEBAPP_IMAGE_URL=$(ECR)/shipreq/webapp
CI_LOCAL_DOCKER_ARGS += -e TASKMAN_IMAGE_URL=$(ECR)/shipreq/taskman
CI_LOCAL_DOCKER_ARGS += -w /Code
CI_TMP_TARGET         = /tmp/sbt-ci-target

info:
	@echo "This Makefile isn't how you build ShipReq (use SBT for that), but rather a collection of helpful utilities."
	@echo
	@echo "Commands:"
	@echo
	@echo "  * build_env       -- Create a new build-env instance and log in to it."
	@echo "  * ci-local        -- Do what CI does but from the local machine (and for real)."
	@echo "  * ci-shell        -- Open a shell to a fresh, local CI env."
	@echo "  * deploy          -- Publish to AWS, start a build, update prod Terraform."
	@echo "  * diff_prod       -- Show a diff between ShipReq that's currently in production, and local HEAD."
	@echo "  * docker-push-dev -- Pushes {webapp,taskman}:latest to ECR tagged as dev"
	@echo "  * prod_ver        -- Show the Git SHA of ShipReq that's currently in production."
	@echo "  * publish         -- Publish to AWS, start a build."
	@echo "  * pull_images     -- Pull ShipReq images from ECR."
	@echo "  * push            -- Git push to origin and Github"
	@echo "  * rm_targets      -- Removes all local target directories."
	@echo "  * since_prod      -- Show the commits that have occurred since the last production deployment."
	@echo "  * watch_codebuild -- Watches CodeBuild status."
	@echo

build_env:
	$$($(AWS) ecr get-login --no-include-email)
	docker pull $(ECR)/shipreq/dev/build_env
	make -f ../Docker/dev-build_env/Makefile exec2

ci-local:
ifneq ($(BRANCH),master)
	$(error You need to be on the master branch, not $(BRANCH))
else ifneq ($(CHANGES),)
	$(error You have unsaved changes)
else
	$$($(AWS) ecr get-login --no-include-email)
	docker pull $(ECR)/shipreq/dev/build_env
	docker pull $(ECR)/shipreq/base
	docker pull $(ECR)/shipreq/taskman
	docker pull $(ECR)/shipreq/webapp
	bin/env test up -d
	rm -rf target && mkdir -p $(CI_TMP_TARGET) && ln -s $(CI_TMP_TARGET) target
	docker run $(CI_LOCAL_DOCKER_ARGS) $(ECR)/shipreq/dev/build_env bin/ci
	git push aws master
	rm -rf target
endif

ci-shell:
	$$($(AWS) ecr get-login --no-include-email)
	docker pull $(ECR)/shipreq/dev/build_env
	docker pull $(ECR)/shipreq/base
	docker pull $(ECR)/shipreq/taskman
	docker pull $(ECR)/shipreq/webapp
	bin/env test up -d
	rm -rf target && mkdir -p $(CI_TMP_TARGET) && ln -s $(CI_TMP_TARGET) target
	@echo
	@echo "You'll likely want to do one of the following:"
	@echo "  - bin/ci"
	@echo "  - bin/ci shell"
	@docker run -it $(CI_LOCAL_DOCKER_ARGS) $(ECR)/shipreq/dev/build_env bash
	rm -rf target

deploy: publish
	../Terraform/prod/update_shipreq.sh
	$(WATCH_CODEBUILD)

diff_prod:
	git diff -w $(PROD_REV)... -- '*.scala' '*.sbt' frontend/{package.json,src,w*}

docker-push-dev:
	$$($(AWS) ecr get-login --no-include-email)
	docker tag shipreq/webapp:latest $(ECR)/shipreq/webapp:dev
	docker tag shipreq/taskman:latest $(ECR)/shipreq/taskman:dev
	docker push $(ECR)/shipreq/webapp:dev
	docker push $(ECR)/shipreq/taskman:dev

prod_ver:
	@echo $(PROD_REV)

publish:
ifneq ($(BRANCH),master)
	$(error You need to be on the master branch, not $(BRANCH))
else ifneq ($(CHANGES),)
	$(error You have unsaved changes)
else
	git push aws master
	aws codebuild start-build --project-name shipreq
endif

pull_images:
	$$($(AWS) ecr get-login --no-include-email)
	docker pull $(ECR)/shipreq/taskman
	docker pull $(ECR)/shipreq/webapp
	docker tag $(ECR)/shipreq/taskman shipreq/taskman
	docker tag $(ECR)/shipreq/webapp shipreq/webapp

push:
	git push
	git push github $(BRANCH)

rm_targets:
	find . -name target -type d | egrep -v './project|frontend' | xargs rm -rfv

since_prod:
	@git log \
	--format='%C(yellow)%h%Creset %ai %C(cyan)<%an>%Creset %s' \
	$(PROD_REV).. \
	-- \
	. \
	':(exclude,glob)**/*.md' \
	':(exclude,glob)**/Makefile' \
	':(exclude,glob)**/frontend/dist'

watch_codebuild:
	$(WATCH_CODEBUILD)
