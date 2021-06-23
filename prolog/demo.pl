% logic programming

% define some predicate.
% or "fact"
magicNumber(7).
magicNumber(9).
magicNumber(42).

% asking question:
% ?- .. is what runs in repl
% is number 7 a magic number?
?- magicNumber(7).    % true
?- magicNumber(8).    % false
?- magicNumber(9).    % false

?- magicNumber(Presto).


?- 2 = 3.   % false - equality test
?- X = 3.   % X = 3 assigment
?- X = 2, X = Y. % two assignment
?- X = 3, X = 2. % false - it has two steps, first assign,
                 % second eq test becasue X is bound
?- X = 3 - 2.    % X = 3+2 - unification can't do arithmetic
?- X is 3 + 2.   % X = 5, is does arithmetic, rhs of is must always be bound

% any unification (any predicate in prolog) can either
% succeed without changing anything
% succeed and bind one or more variables in the proces
%         (bc an assignment style unification was made true)
% fail  bc an equality style unification was false

?- plus(1, 2, 3).   % true
?- plus(1, 2, X).   % X = 3 because 1 + 2 = X
?- plus(1, X, 3).   % X = 2 because 1 + X = 3
?- plus(X, 5, Y).   % error The solution set is infinite

% magicNumber is a predicate we already defined.
% now solve the following unification
?- magicNumber(X), plus(X, Y, 100). % X = 7, Y = 93;
                                    % X = 9, Y = 91;
                                    % X = 42, Y = 58.

?- magicNumber(X), X > 40.   % X = 42
?- magicNumber(X), X > 100.  % false - no solution

% print always succeeds, never binds any variables, and has side effect.
?- print("Hello").  % "Hello" true
?- X = 2, print(X). % 2 true
?- X = 2, print(X), X = 3. % 2 false, print still executes and perform the side effect


