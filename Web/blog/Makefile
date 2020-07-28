GATSBY := ./node_modules/gatsby-cli/cli.js
TF_DIR := ../../Terraform/web
TF_OUT := ./src/config/terraform.ts


.PHONY: clean cd cp dev prod dev-build prod-build watch-prod sync

default: dev

clean:
	$(GATSBY) clean

dev:
	$(GATSBY) develop

dev-build:
	$(GATSBY) build

prod-build:
	ENV=prod $(GATSBY) build
	find public -name '*.map' -delete

prod: prod-build
	$(GATSBY) serve

cd: clean dev
cp: clean prod

watch-prod: clean
	git ls-files | entr -cr make prod-build

sync:
	echo '// Re-generate this by running "make sync"' > $(TF_OUT)
	< $(TF_DIR)/blog.tf grep ' domain *=' | perl -pe 's/.*(".+").*/export const hostname = \1/' >> $(TF_OUT)
	< $(TF_DIR)/blog.tf fgrep s3_bucket_name | perl -pe 's/.*(".+").*/export const s3_bucket = \1/' >> $(TF_OUT)
	< $(TF_DIR)/setup.tf fgrep blog/gatsy | fgrep ' region ' | perl -pe 's/.*(".+").*/export const s3_region = \1/' >> $(TF_OUT)

deploy:
	./node_modules/gatsby-plugin-s3/bin.js deploy

ci: clean prod-build deploy
