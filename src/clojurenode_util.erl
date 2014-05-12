-module(clojurenode_util).

-export([start/0]).

start() ->
    application:load(clojurenode),
    application:start(clojurenode).

