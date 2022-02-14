-module(counter).
-export([start/0, counter_loop/1, value/1, increment/1, stop/1]).

% a counter process that allows us to increment, access it's value, and stop.

start() ->
    spawn(counter, counter_loop, [0]).

stop(Counter) ->
    Counter ! stop.

value(Counter) ->
    Counter ! {self(), value},
    receive
        {Counter, Value} ->
            Value
    end.

increment(Counter) ->
    Counter ! increment.

counter_loop(Val) ->
    receive
        increment ->
            counter_loop(Val + 1);
        {From, value} ->
            From ! {self(), Val},
            counter_loop(Val);
        stop ->
            true;
        _Other ->
            counter_loop (Val)
    end.
