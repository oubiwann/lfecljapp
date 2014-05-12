-module(lfecljapp_util).

-export([start/0, ping/3]).

start() ->
    application:load(lfecljapp),
    application:start(lfecljapp).

ping(MBox, Recip, Sender) ->
    erlang:send({MBox, Recip}, {ping, Sender}).
