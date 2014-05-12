EXPECTED_APP_NAME = clojurenode
LINK = ../$(EXPECTED_APP_NAME)
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

$(LINK):
	-ln -s erlang-clojure-node $(LINK)

shell: $(LINK)
	cd ../$(EXPECTED_APP_NAME)/ && \
	erl -sname erl_node@$(HOST) -pa `pwd`/ebin

dev: $(LINK)
	cd ../$(EXPECTED_APP_NAME)/ && \
	erl -sname erl_node@$(HOST) -pa `pwd`/ebin -s clojurenode_util
