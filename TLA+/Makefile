TLC      := ./tlc -difftrace
TLC1     := TLC_OPTS="-workers 1" $(TLC)
SHOW_SEP := @echo -e "\e[46m========================================================================================================================\e[0m"
SHARED   := convert_dialect Makefile tlc Util.tla

info:
	@echo "Commands:"
	@echo
	@echo "  * drafts[-watch]"
	@echo "  * encryption[-watch]"
	@echo "  * project[-watch]"
	@echo "  * websocket_client[-watch]"
	@echo "  * websockets[-watch]"
	@echo


.PHONY: drafts drafts-watch
drafts:
	$(SHOW_SEP)
	$(TLC) -deadlock drafts
drafts-watch:
	ls -1 drafts* $(SHARED) | entr -cr make drafts


.PHONY: encryption encryption-watch
encryption:
	$(TLC) encryption
encryption-watch:
	ls -1 encryption* $(SHARED) | entr -cr make encryption


.PHONY: project project-watch
project:
	./project
project-watch:
	ls -1 project* $(SHARED) | entr -cr make project


.PHONY: websocket_client websocket_client-watch
websocket_client:
	$(TLC) websocket_client
websocket_client-watch:
	ls -1 websocket_client* $(SHARED) | entr -cr make websocket_client


.PHONY: websockets websockets-watch
websockets:
	$(TLC) websockets
websockets-watch:
	ls -1 websockets* $(SHARED) | entr -cr make websockets
