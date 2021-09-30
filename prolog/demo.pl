% logic programming:
%
% program statements express the facts and rules about different problem within
% a system of formal logic.
%
% Rules are written in logical clause (horn clauses), each clause has a head H
% and multiple bodies B1, B2 ..
% So we say: H is true when B1, B2, B3 ... are all true.
% It's like haskell's constraint solver.

% define a main program
main :- write('Hello world\n'),
        write('More stuffs').

%%%%%% basic

%% facts: explicit relation between object
% here we list some facts:
cat(tom).
love_to_eat(kunal, pasta).
of_color(hair, black).
love_to_play_games(nawaz).
lazy(pratyusha).

%% rules:  lili is happy IF lili dances

% define some predicates first.
dances(lili).
search_for_food(n).

joy(lili) :- dances(lili).
hungry(tom) :- search_for_food(tom).
friends(jack, bili) :- loves_cricket(jack), loves_cricket(bili).
go_to_play(ryan) :- is_closed(school), free(ryan).

% queries:

% knowledge base: well it's just a collection of facts and rules.

pp(priya).
pp(tiyasha).
pp(jaya).
can_cook(priya).

who_can_cook_main :- pp(priya).

sing_a_song(ananya).
listen_to_music(rohit).
listen_to_music(ananya) :- sing_a_song(ananya).
happy(ananya) :- sing_a_song(ananya).
happy(rohit) :- listen_to_music(rohit).
play_guitar(rohit) :- listen_to_music(rohit).

sing_song_main :- happy(rohit).

%%%%%% relations

%%%%%% data objects (just some datatypes)
% prolog supports atom, number, variables, structures.

%%%%%% looping / control_flow
% just a simple recursion.
count_to_20(20) :- write(20), nl.
count_to_20(X) :-
  write(X), nl,
  Y is X + 1,
  count_to_20(Y).

% press ; to get more answer.
count_down(L, H) :-
  between(L, H, Y),
  Z is H - Y,
  write(Z), nl.

count_up(L, H) :-
  between(L, H, Y),
  Z is L + Y,
  write(Z), nl.

% decision making.
% this really just pattern matching + guard.
gt(X, Y) :- X >= Y, write("X is greater or equal").
gt(X, Y) :- X < Y, write("X is smaller").

gte(X, Y) :- X > Y, write("X is greater").
gte(X, Y) :- X = Y, write("X is equalto Y").
gte(X, Y) :- X < Y, write("X is smaller").

% conjunction and disjunction
boss(jhon, bob).
boss(lili, bob).

male(jhon).
female(lili).

male_boss(X, Y) :- boss(X, Y), male(X).
female_boss(X, Y) :- boss(X, Y), female(X).

% some longer definition.
who_is_the_boss(X, Gender) :-
  Gender = male,
  boss(Y, X),
  write(Y).
who_is_the_boss(X, Gender) :-
  Gender = female,
  female_boss(Y, X),
  write(Y).

employee_of(X, Y) :- male_boss(X, Y); female_boss(X, Y).

% list.
list_member(X, [X|_]).
list_member(X, [_|Tail]) :- list_member(X, Tail).

is_b_member_of_list :- list_member(b, [a, b, c, 1]).

list_length([], 0).
list_length([_|Tail], N) :- list_length(Tail, N1), N is N1 + 1.

%  checking whether the list is even or odd.
%  similar technique can be equipped by GADT and type level proofs.
list_even_len([]).
list_even_len([_|Tail]) :- list_odd_len(Tail).

list_odd_len([_]).
list_odd_len([_|Tail]) :- list_even_len(Tail).

% in logic programming parameters can be return value, return values can be
% parameter. As long as the result conform the logic system it's good to go.
% this example shows it very clearly.
list_divide([], [], []).
list_divide([X], [X], []).
list_divide([X, Y|Tail], [X|List1], [Y|List2]) :- list_divide(Tail, List1, List2).


max_of_two(X, Y, X) :- X >= Y.
max_of_two(X, Y, Y) :- X < Y.
list_max_elem([X], X).
list_max_elem([X, Y|Rest], Max) :-
  list_max_elem([Y|Rest], MaxRest),
  max_of_two(X, MaxRest, Max).

list_sum([], 0).
list_sum([Head|Tail], Sum) :-
  list_sum(Tail, SumTmp),
  Sum is Head + SumTmp.

% merge sort

mergesort([], []).
mergesort([A], [A]).
mergesort([A, B|R], S) :-
  split([A, B|R], L1, L2),
  mergesort(L1, S1),
  mergesort(L2, S2),
  merge(S1, S2, S).

split([], [], []).
split([A], [A], []).
split([A, B|R], [A|Ra], [B|Rb]) :- split(R, Ra, Rb).

merge(A, [], A).
merge([], B, B).
merge([A|Ra], [B|Rb], [A|M]) :- A =< B, merge(Ra, [B|Rb], M).
merge([A|Ra], [B|Rb], [B|M]) :- A > B, merge([A|Ra], Rb, M).

%%%%% recursion structure.

%%%%% back tracking.

girl(priya).
girl(tiyasha).
girl(jaya).

boy(bob).
boy(tom).

% this will need to backtrack.
pay(X, Y) :- boy(X), girl(Y).

% we can use cut to cut off back tracaking.

% this three rules are mutally exclusive to each other. Once we find a solution
% for one of them there is no need to proceed.
we_can_cut_this(X, 0) :- X < 3, !.
we_can_cut_this(X, 2) :- X >= 3, X < 6, !.
we_can_cut_this(X, 4) :- X >= 6, !.

animal(dog).
animal(cat).
animal(elephant).
animal(tigher).
animal(cobra).
animal(python).

snake(cobra).
snake(python).

likes(mary, X) :- snake(X), !, fail.
likes(mary, X) :- animal(X).

not(P) :- P, !, fail; true.

cube :-
  write("Write a number: "),
  read(Number), nl,
  process(Number).

process(stop) :- !.
process(Number) :-
  C is Number * Number * Number,
  write("Cube of "), write(Number), write(": "), write(C), nl, cube.
