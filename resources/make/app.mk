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
	make -C cljnode --no-print-directory
	mv cljnode/target/*.jar priv

erlang: get-deps clean-ebin
	@echo "Compiling project code and dependencies ..."
	@which rebar.cmd >/dev/null 2>&1 && rebar.cmd compile || rebar compile

erlang-no-deps: clean-ebin
	@echo "Compiling only project code ..."
	@which rebar.cmd >/dev/null 2>&1 && rebar.cmd compile skip_deps=true || rebar compile skip_deps=true

compile: clojure erlang

compile-no-deps: clojure erlang-no-deps

shell:
	erl -sname erl_node@$(HOST) -pa `pwd`/ebin

dev:
	erl -sname erl_node@$(HOST) -pa `pwd`/ebin -s lfecljapp_util

repl: compile
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting shell ..."
	@PATH=$(SCRIPT_PATH) lfetool repl

repl-no-deps: compile-no-deps
	@which clear >/dev/null 2>&1 && clear || printf "\033c"
	@echo "Starting shell ..."
	@PATH=$(SCRIPT_PATH) lfetool repl

clean: clean-ebin clean-eunit
	@-which rebar.cmd >/dev/null 2>&1 && rebar.cmd clean || rebar clean
	-rm priv/*.jar
	-rm erl_crash.dump
