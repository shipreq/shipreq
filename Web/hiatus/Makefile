.PHONY: build clean deploy serve

AWS           := aws --profile=shipreq
CLOUDFRONT_ID := E1UUBCK870MNWS
DIST          := dist
PID_FILE      := /tmp/shipreq-hiatus.pid
S3_BUCKET     := shipreq-prod-altsite
STATIC        := static

info:
	@echo "ShipReq on Hiatus."
	@echo
	@echo "Commands:"
	@echo
	@echo "  * build  — Builds the site in prod mode"
	@echo "  * clean  — Clean up intermediate files"
	@echo "  * deploy — Deploys to production"
	@echo "  * serve  — Starts the dev server"
	@echo

serve:
	@npm run start

clean:
	@rm -rf $(DIST) $(PID_FILE)

build:
	@echo "Rebuilding $(DIST) ..."
	@rm -rf $(DIST)
	@cp -pr $(STATIC) $(DIST)
	@NODE_ENV=production ./node_modules/.bin/node-dev server.js >/dev/null & echo "$$!" > $(PID_FILE)
	@sleep 1
	@curl http://localhost:3000/index -so $(DIST)/index.html
	@cat $(PID_FILE) | xargs kill
	@rm -f $(PID_FILE)

deploy: build
	@echo
	@echo "Uploading ..."
	@$(AWS) s3 sync --delete --exact-timestamps $(DIST) s3://$(S3_BUCKET)
	@echo
	@echo "Invalidating CDN ..."
	@$(AWS) cloudfront create-invalidation --distribution-id $(CLOUDFRONT_ID) --paths "/*"
