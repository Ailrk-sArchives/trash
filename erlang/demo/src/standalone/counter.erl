-module(counter).
-export([start/0, loop/2]).

start() ->
    spawn(counter, loop, [0, 10]).

loop(Val, Bound) ->
    if
        Val >= Bound -> true;
        _ -> receive
                 increment -> loop(Val + 1)
             end
    end.
