-module(withlist).
-export([qsort/1]).

qsort([]) -> [];
qsort([Pivot|Rest]) ->
    {Smaller, Bigger} = split(Pivot, Rest),
    lists:append(qsort(Smaller), [Pivot|qsort(Bigger)]).

split(Pivot, L) -> split(Pivot, L, [], []).

split(_, [], Smaller, Bigger) -> {Smaller, Bigger};
split(Pivot, [H|T], Smaller, Bigger) when H < Pivot ->
    split(Pivot, T, [H|Smaller], Bigger);
split(Pivot, [H|T], Smaller, Bigger) when H >= Pivot ->
    split(Pivot, T, Smaller, [H|Bigger]).
