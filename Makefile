EXPECTED_APP_NAME = clojurenode
OS := $(shell uname -s)
ifeq ($(OS),Linux)
	HOST=$(HOSTNAME)
endif
ifeq ($(OS),Darwin)
	HOST = $(shell scutil --get ComputerName)
endif

clojure:
	-mkdir priv
	make -C cljnode --no-print-directory
	mv cljnode/target/*.jar priv

erlang:
	rebar compile

compile: clojure erlang

link:
	-ln -s erlang-clojure-node ../$(EXPECTED_APP_NAME)

shell: link
	cd ../$(EXPECTED_APP_NAME)/ && \
	erl -pa `pwd`/ebin

dev: link
	cd ../$(EXPECTED_APP_NAME)/ && \
	erl -sname erl_node@$(HOST) -pa `pwd`/ebin -s clojurenode_util
