.PHONY: info deploy prod_diff prod_ver publish push rm_targets since_prod

BRANCH   := $(shell git rev-parse --abbrev-ref HEAD)
CHANGES  := $(shell git status --porcelain)
PROD_REV := $(shell fgrep shipreq ../Infrastructure/terraform/prod/versions.tf | perl -pe 's/.*git-(.+?)".*/$$1/')

info:
	@echo "This Makefile isn't how you build ShipReq, but rather a collection of helpful utilities."
	@echo
	@echo "Commands:"
	@echo
	@echo "  * deploy     -- Publish to AWS, start a build, update prod Terraform."
	@echo "  * prod_diff  -- Show a diff between ShipReq that's currently in production, and local HEAD."
	@echo "  * prod_ver   -- Show the Git SHA of ShipReq that's currently in production."
	@echo "  * publish    -- Publish to AWS, start a build."
	@echo "  * push       -- Git push to origin and Github"
	@echo "  * rm_targets -- Removes all local target directories."
	@echo "  * since_prod -- Show the commits that have occurred since the last production deployment."
	@echo

deploy: publish
	../Infrastructure/terraform/prod/update_shipreq.sh

prod_diff:
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

push:
	git push
	git push github $(BRANCH)

rm_targets:
	find . -name target -type d | egrep -v './project|frontend' | xargs rm -rfv

since_prod:
	git l $(PROD_REV).. .
