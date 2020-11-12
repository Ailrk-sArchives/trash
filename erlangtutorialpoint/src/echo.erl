-module(echo).
-export([start/0, loop/0]).

% an echo process
start() ->
    spawn(echo, loop, []).

% to talk with it just send a message {From, msg} to it.
loop() ->
    receive
        {From, Message} ->
            From ! Message,
            loop()
    end.
