.PHONY: build_env info deploy diff_prod prod_ver publish pull_images push rm_targets since_prod watch_codebuild

BRANCH   := $(shell git rev-parse --abbrev-ref HEAD)
CHANGES  := $(shell git status --porcelain)
PROD_REV := $(shell fgrep shipreq ../Terraform/prod/versions.tf | perl -pe 's/.*git-(.+?)".*/$$1/')
ECR      := 930801536115.dkr.ecr.ap-southeast-2.amazonaws.com

WATCH_CODEBUILD = watch -n 6 -c aws-codebuild-status -t 'Name:shipreq'

info:
	@echo "This Makefile isn't how you build ShipReq, but rather a collection of helpful utilities."
	@echo
	@echo "Commands:"
	@echo
	@echo "  * build_env       -- Create a new build-env instance and log in to it."
	@echo "  * deploy          -- Publish to AWS, start a build, update prod Terraform."
	@echo "  * diff_prod       -- Show a diff between ShipReq that's currently in production, and local HEAD."
	@echo "  * prod_ver        -- Show the Git SHA of ShipReq that's currently in production."
	@echo "  * publish         -- Publish to AWS, start a build."
	@echo "  * pull_images     -- Pull ShipReq images from ECR."
	@echo "  * push            -- Git push to origin and Github"
	@echo "  * rm_targets      -- Removes all local target directories."
	@echo "  * since_prod      -- Show the commits that have occurred since the last production deployment."
	@echo "  * watch_codebuild -- Watches CodeBuild status."
	@echo

build_env:
	$$(aws ecr get-login --no-include-email)
	docker pull $(ECR)/shipreq/dev/build_env
	make -f ../Docker/dev-build_env/Makefile exec2

deploy: publish
	../Terraform/prod/update_shipreq.sh
	$(WATCH_CODEBUILD)

diff_prod:
	git diff -w $(PROD_REV)... -- '*.scala' '*.sbt' frontend/{package.json,src,w*}

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
	$$(aws ecr get-login --no-include-email)
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
