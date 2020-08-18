.PHONY: build c cd ci clean cp d deploy dev invalidate-prod p prod sync watch-prod

GATSBY        := ./node_modules/gatsby-cli/cli.js
GATSBY_s3     := ./node_modules/gatsby-plugin-s3/bin.js
TF_DIR        := ../../Terraform/blog
TF_OUT        := ./src/config/terraform.ts
CLOUDFRONT_ID := E1VOYBWVFSWD3V

info:
	@echo "ShipReq Blog."
	@echo
	@echo "Commands:"
	@echo
	@echo "  * build           -- Builds the site in prod mode"
	@echo "  * ci              -- Builds and deploys from CI"
	@echo "  * clean           -- gatsby clean"
	@echo "  * deploy          -- Deploys what's build to S3"
	@echo "  * dev             -- gatsby develop"
	@echo "  * invalidate-prod -- Invalidate everything in CloudFront"
	@echo "  * prod            -- gatsby build && gatsby serve"
	@echo "  * sync            -- Synchronise config here with the latest values from Terraform"
	@echo "  * watch-prod      -- Watches for changes and builds prod"
	@echo

# Aliases
b : build
c : clean
cd: clean dev
cp: clean prod
d : dev
p : prod
wp: watch-prod

clean:
	$(GATSBY) clean
	@echo

dev:
	$(GATSBY) develop --host 0.0.0.0

build:
	$(GATSBY) build
	@echo

prod: build
	$(GATSBY) serve --host 0.0.0.0

watch-prod: clean
	git ls-files | entr -cr make build

deploy:
	find public -name '*.map' -delete
	$(GATSBY_s3) deploy

invalidate-prod:
	aws cloudfront create-invalidation --distribution-id $(CLOUDFRONT_ID) --paths "/*"

ci: clean build deploy invalidate-prod

sync:
	echo '// Re-generate this by running "make sync"' > $(TF_OUT)
	< $(TF_DIR)/blog.tf grep ' domain *=' | perl -pe 's/.*(".+").*/export const hostname = \1/' >> $(TF_OUT)
	< $(TF_DIR)/blog.tf fgrep s3_bucket_name | perl -pe 's/.*(".+").*/export const s3_bucket = \1/' >> $(TF_OUT)
	< $(TF_DIR)/setup.tf fgrep blog/gatsy | fgrep ' region ' | perl -pe 's/.*(".+").*/export const s3_region = \1/' >> $(TF_OUT)
