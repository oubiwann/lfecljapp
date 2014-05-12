OS := $(shell uname -s)
ifeq ($(OS),Linux)
	HOST=$(HOSTNAME)
endif
ifeq ($(OS),Darwin)
	HOST=$(shell scutil --get ComputerName)
endif

clojure:
	-mkdir priv
	make -C cljnode --no-print-directory
	mv cljnode/target/*.jar priv

compile:
	rebar compile

all: clojure compile

link:
	-ln -s erlang-clojure-node ../clojurenode

shell: link
	cd ../clojurenode/ && \
	erl -pa `pwd`/ebin

dev: link
	cd ../clojurenode/ && \
	erl -sname erl_node@$(HOST) -pa `pwd`/ebin -s clojurenode_util
