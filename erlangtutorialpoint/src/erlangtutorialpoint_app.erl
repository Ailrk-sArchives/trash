%%%-------------------------------------------------------------------
%% @doc erlangtutorialpoint public API
%% @end
%%%-------------------------------------------------------------------

-module(erlangtutorialpoint_app).

-behaviour(application).

-export([start/2, stop/1]).



start(_StartType, _StartArgs) ->
    io:fwrite("hello, world"),
    erlangtutorialpoint_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
