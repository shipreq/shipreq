MAKE      := make -s
TLC       := ./tlc -difftrace
TLC1      := TLC_OPTS="-workers 1" $(TLC)
TLCs      := $(TLC1) -simulate -seed 0
SHOW_SEP  := @echo -e "\e[46m========================================================================================================================\e[0m"
SHARED    := convert_dialect Makefile tlc Util.tla
TLA_QUERY := /home/golly/projects/public/tla-query/run -sl

info:
	@echo "Commands:"
	@echo
	@echo "  * drafts-last"
	@echo "  * drafts[-trace][-watch]"
	@echo "  * encryption[-watch]"
	@echo "  * jq-test[T]"
	@echo "  * project[-watch]"
	@echo "  * websocket_client[-watch]"
	@echo "  * websockets[-watch]"
	@echo


.PHONY: drafts drafts-watch drafts-trace drafts-trace-watch drafts-last
drafts:
	$(SHOW_SEP)
	@$(TLC) drafts | tee drafts.log
	@fgrep -q 'State 1:' drafts.log && echo && $(MAKE) drafts-trace || test 1
drafts-watch:
	ls -1 drafts.{cfg,tla} $(SHARED) | entr -cr $(MAKE) drafts
drafts-trace:
	@$(TLA_QUERY) drafts.log | jq -rf drafts.jq | column -t -s$$'\t'
drafts-trace-watch:
	ls -1 drafts.{log,jq} $(SHARED) | entr -cr $(MAKE) drafts-last
drafts-last:
	@sed '0,/^Starting\.\.\./d' <drafts.log
	@echo
	@$(MAKE) drafts-trace


.PHONY: encryption encryption-watch
encryption:
	$(TLC) encryption
encryption-watch:
	ls -1 encryption.{cfg,tla} $(SHARED) | entr -cr $(MAKE) encryption


.PHONY: jq-test jq-testT
jq-test:
	ls -1 jq-test.{jq,json} $(SHARED) | entr -crs "jq -cf jq-test.jq jq-test.json"
jq-testT:
	ls -1 jq-test.{jq,json} $(SHARED) | entr -crs "jq -rcf jq-test.jq jq-test.json | column -t -s$$'\t'"


.PHONY: project project-watch
project:
	./project
project-watch:
	ls -1 project project.{cfg,tla} $(SHARED) | entr -cr $(MAKE) project


.PHONY: websocket_client websocket_client-watch
websocket_client:
	$(TLC) websocket_client
websocket_client-watch:
	ls -1 websocket_client.{cfg,tla} $(SHARED) | entr -cr $(MAKE) websocket_client


.PHONY: websockets websockets-watch
websockets:
	$(TLC) websockets
websockets-watch:
	ls -1 websockets.{cfg,tla} $(SHARED) | entr -cr $(MAKE) websockets
