-module(fsm_main).
-export([start/0]).

start() ->
    S <- fsm:start().
