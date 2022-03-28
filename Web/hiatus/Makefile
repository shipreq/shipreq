.PHONY: build clean serve

DIST     := dist
STATIC   := static
PID_FILE := /tmp/shipreq-hiatus.pid

info:
	@echo "ShipReq on Hiatus."
	@echo
	@echo "Commands:"
	@echo
	@echo "  * build — Builds the site in prod mode"
	@echo "  * clean — Clean up intermediate files"
	@echo "  * serve — Starts the dev server"
	@echo

serve:
	@npm run start

clean:
	@rm -rf $(DIST) $(PID_FILE)

build:
	@rm -rf $(DIST)
	@cp -pr $(STATIC) $(DIST)
	@NODE_ENV=production ./node_modules/.bin/node-dev server.js >/dev/null & echo "$$!" > $(PID_FILE)
	@sleep 1
	@curl http://localhost:3000/index -so $(DIST)/index.html
	@cat $(PID_FILE) | xargs kill
	@rm -f $(PID_FILE)
