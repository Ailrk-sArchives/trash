-module(keep_alive).
-export([start/0, start/1, start1/1, new_process/3, loop/1]).
-export_type([process_ctx/0]).

-type process_ctx() :: {pid(), atom(), fun(), [any()]}.

start() ->
    register(keep_alive, spawn(keep_alive, start1, [[]])).
start(Processes) ->
    register(keep_alive, spawn(keep_alive, start1, Processes)).


start1([]) ->
    process_flag(trap_exit, true),
    loop([]);
start1(Processes) ->
    process_flag(trap_exit, true),
    loop(Processes).


% maintain a list of processes that should be restarted if dieded.
-spec loop([process_ctx]) -> none().
loop(Processes) ->
    receive
        {From, {new_proc, Mod, Func, Args}} ->
            Id = spawn_link(Mod, Func, Args),
            From ! {keep_alive, started},
            loop([{Id, Mod, Func, Args}|Processes]);

        {'EXIT', Id, _} ->
            case lists:keysearch(Id, 1, Processes) of
                false ->
                    loop(Processes);
                {value, {Id, Mod, Func, Args}} ->
                    P = lists:delete({Id, Mod, Func, Args}, Processes),
                    Id1 = spawn_link(Mod, Func, Args),
                    loop([{Id1, Mod, Func, Args} | P])
            end
    end.

-spec new_process(any(), fun(), [any()]) -> boolean().
new_process(Mod, Func, Args) ->
    keep_alive ! {self(), {new_proc, Mod, Func, Args}},
    receive
        {keep_alive, started}
        -> true
    end.
