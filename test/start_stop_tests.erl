-module(start_stop_tests).

-include_lib("eunit/include/eunit.hrl").
-include_lib("common/include/test.hrl").

-define(APPS, [ sasl ]).

%% =============================================================================
%%  Common
%% =============================================================================
setup()->
    error_logger:tty(false),
    tools:make_distrib("test_node", shortnames),
    ?START_APPS(
        ?APPS, [
            {sasl, [ {sasl_error_logger, {file, "test.log"}} ]}
            ]
        ),
    ok.

cleanup(_)->
    ?STOP_APPS(?APPS),
    tools:stop_distrib(),
    error_logger:tty(true),
    ok.

%% =============================================================================
%%  Tests
%% =============================================================================
main_test_() ->
    ?FIXTURE(
        fun()->
            ?assertEqual(ok, application:load(clojurenode)),
            ?assertEqual(ok, application:start(clojurenode)),
            %% wait for java start
            timer:sleep(3000),
            ?assertEqual(
                {error,{already_started, clojurenode}},
                application:start(clojurenode)
                ),
            ?assertEqual(ok, application:stop(clojurenode)),
            ?assertEqual(ok, application:unload(clojurenode)),
            ok
        end
        ).

%% =============================================================================
%%  Helpers
%% =============================================================================
