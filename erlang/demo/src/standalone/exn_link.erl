-module(exn_link).
-export([start/1, p1/1, passdown/2, top/0, top/2]).

% processors monitor each other's behaviours with links and EXIT signals.
%
% start <--> top

start(N) ->
    register(start, spawn_link(exn_link, p1, [N - 1])).

p1(0) ->
    top();
p1(N) ->
    top(spawn_link(exn_link, p1, [N - 1]), N).

top(Next, N) ->
    receive
        X ->
            Next ! X,        % pass to the next process.
            io:format("Process ~w received ~w~n", [N, X]),
            top(Next, N)
    end.

top() ->
    receive
        stop ->
            io:format("Last process is exiting ~n", []),
            exit(finished);
        X ->
            io:format("Last process received ~w~n", [X]),
            top()
    end.

passdown(Mess) ->
    start ! Mess.
