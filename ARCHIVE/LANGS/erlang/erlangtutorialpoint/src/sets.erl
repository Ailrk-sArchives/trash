-module(sets).
-export([new/0, add/2, del/2, is_element/2, is_empty/1, union/2, intersection/2]).

new() -> [].

add(X, Set) -> case is_element(X, Set) of
                   true -> [X|Set];
                   false -> Set
               end.

del(X, [X|T]) -> T;
del(X, [Y|T]) -> [Y|del(X, T)];
del(_, []) -> [].

is_element(H, [H|_]) -> true;
is_element(H, [_|Set]) -> is_element(H, Set);
is_element(_, []) -> false.

is_empty(L) -> case L of
                   [] -> true;
                   _ -> false
               end.

union([H|T], Set) -> union(T, add(H, Set));
union([], Set) -> Set.

intersection(S1, S2) -> intersection(S1, S2, []).
intersection([], _, S) -> S;
intersection([H|T], S1, S) ->
    case is_element(H, S1) of
        true -> intersection(T, S1, [H|S]);
        false -> intersection(T, S1, S)
    end.
