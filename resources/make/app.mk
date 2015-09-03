OS := $(shell uname -s)
ifeq ($(OS),Linux)
	HOST=$(HOSTNAME)
endif
ifeq ($(OS),Darwin)
	HOST = $(shell scutil --get ComputerName)
endif

priv:
	mkdir priv

clojure: priv
	@lein compile
	@lein uberjar
	mv target/*.jar priv
	mkdir -p log

erlang: get-deps clean-ebin
	@echo "Compiling project code and dependencies ..."
	@which rebar.cmd >/dev/null 2>&1 && rebar.cmd compile || rebar compile
	@ERL_LIBS=$(ERL_LIBS) \
	PATH=$(SCRIPT_PATH):deps/lfe/bin lfec -o $(OUT_DIR) src/lfe/*.lfe
	@cp src/lfe/lfecljapp.app.src $(OUT_DIR)/lfecljapp.app

erlang-no-deps: clean-ebin
	@echo "Compiling only project code ..."
	@ERL_LIBS=$(ERL_LIBS) \
	PATH=$(SCRIPT_PATH):deps/lfe/bin lfec -o $(OUT_DIR) src/lfe/*.lfe
	@cp src/lfe/lfecljapp.app.src $(OUT_DIR)/lfecljapp.app

compile: clojure erlang

compile-no-deps: clojure erlang-no-deps

dev:
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting shell ..."
	@ERL_LIBS=$(ERL_LIBS) \
	PATH=$(SCRIPT_PATH) ./deps/lfe/bin/lfe -sname "lfenode@$(HOST)" \
	-s 'lfeclj-app' -pa "`pwd`/$(OUT_DIR)"

repl: erlang
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting shell ..."
	@ERL_LIBS=$(ERL_LIBS) \
	PATH=$(SCRIPT_PATH) ./deps/lfe/bin/lfe -sname "lfenode@$(HOST)" \
	-pa "`pwd`/$(OUT_DIR)"

repl-no-deps: erlang-no-deps
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting shell ..."
	@ERL_LIBS=$(ERL_LIBS) \
	PATH=$(SCRIPT_PATH) lfetool repl lfe -sname lfenode@$(HOST) \
	-pa `pwd`/$(OUT_DIR)

clean: clean-ebin clean-eunit
	@-which rebar.cmd >/dev/null 2>&1 && rebar.cmd clean || rebar clean
	-rm priv/*.jar
	-rm erl_crash.dump
