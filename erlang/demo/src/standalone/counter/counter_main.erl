-module(counter_main).

-export([start/0, signal/3]).

start() ->
    Counter = counter:start(),
    spawn(counter_main, signal, [Counter, 0, 10]).

signal(Counter, Val, Bound) ->
    if
        Val >= Bound ->
            counter:stop(Counter);
        true ->
            io:format("Counter Value: ~w~n", [counter:value(Counter)]),
            counter:increment(Counter),
            timer:sleep(100),
            signal(Counter, Val + 1, Bound)
    end.
