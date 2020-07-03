.PHONY: info deploy publish push rm_targets

BRANCH  := $(shell git rev-parse --abbrev-ref HEAD)
CHANGES := $(shell git status --porcelain)

info:
	@echo "This Makefile isn't how you build ShipReq, but rather a collection of helpful utilities."
	@echo
	@echo "Commands:"
	@echo
	@echo "  * deploy     -- Publish to AWS, start a build, update prod Terraform."
	@echo "  * publish    -- Publish to AWS, start a build."
	@echo "  * push       -- Git push to origin and Github"
	@echo "  * rm_targets -- Removes all local target directories."
	@echo

deploy: publish
	../Infrastructure/terraform/prod/update_shipreq.sh

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
