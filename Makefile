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

erlang:
	rebar compile

compile: clojure erlang

shell:
	erl -sname erl_node@$(HOST) -pa `pwd`/ebin

dev:
	erl -sname erl_node@$(HOST) -pa `pwd`/ebin -s lfecljapp_util
