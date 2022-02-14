-module(robust_server).
-export([start_server/1]).


% process1 allocate and free resource, and give it to it's caller.
% if process1 is terminated  while the caller holds the resource, the
% resource is dangling.
%
% To avoid this situation we can link process1 with it's caller, and
% set caller to trap EXIT signal.
%
% Doing so, whenever process1 is terminated, EXIT signal will propagated
% as a message to the caller.


start_server(Resources) ->
    process_flag(trap_exit, true),
    server(Resources, []).


server(Free, Allocated) ->
    receive
        {From, alloc} ->
            allocate(Free, Allocated, From);
        {From, {free, R}} ->
            free(Free, Allocated, From, R);
        {'EXIT', From, _} ->
            check(Free, Allocated, From)
    end.


allocate([R|Free], Allocated, From) ->
    link(From),
    From ! {resource_alloc, {yes, R}},
    server(Free, [{R, From} | Allocated]);
allocate([], Allocated, From) ->
    From ! {resource_alloc, no},
    server([], Allocated).


free(Free, Allocated, From, R) ->
    case lists:member({R, From}, Allocated) of
        true ->
            From ! {resource_alloc, yes},
            Allocated1 = lists:delete({R, From}, Allocated),
            case lists:keysearch(From, 2, Allocated1) of
                false ->
                    unlink(From);
                _ ->
                    true
            end,
            server([R|Free], Allocated1);
        false ->
            From ! {resource_alloc, error},
            server(Free, Allocated)
    end.


check(Free, Allocated, From) ->
    case lists:keysearch(From, 2, Allocated) of
        false ->
            server(Free, Allocated);
        {value, {R, From}} ->
            check([R|Free], lists:delete({R, From}, Allocated), From)
    end.
