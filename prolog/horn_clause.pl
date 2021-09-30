%% Hore clause
use_module(libray(lists)).

% First some back grounds on predicate logic.
%   1. atomic formula: has form P(t1, ..., tn) where P is a predicate and tk
%      is a term.
%      e.g M(x), P(x, f(y))
%      note: P(x) -> M(x) is not a atomic formula.
%   2. literal: an atomic formula or a negated atomic formula
%   3. a clause is a disjunction of literals.
%       e.g    L₁ ∨ L₂ ∨ L₃ .. ∨ Lₙ where each Lₖ is a literal.
%
%   4. a horn clause is a clause with at most one positive literal.
%   5. There a several possible kinds of hore claues.
%       1. definite clause.   Horn clause with 1 positive literal
%       2. unit clause.       A definite clause with no negative literal.
%       2. goal lclause.      Horn clause with 0 positive literal.

% Definite clause:
%      H ∨ ~G₁ ∨ ~G₂ ∨ ... ∨ ~Gₙ
%   ≡  H ∨ ~(G₁ ∧ G₂ ∧ ... ∧ Gₙ)
%   ≡  H ← (G₁ ∧ G₂ ∧ ... ∧ Gₙ)
%
%  Thus we can read a definite clause as:
%     G1 and G2 and ... and Gn implies H.
%
%   H is head, Gₖ ... is the body.

% facts.
g1.
g2.
g3 :- true.

head :- g1, g2, g3.

% horn clauses for programming
%   what follows from what? what is the logic consequences of what?
%
%   as programmer we build theories on our domain of interest,
%   so the consequences of the program must align with the intended situation.


% horn clauses are turning complete subset of first order predicate logic!

%% proof prolog is turing complete by building a universal turing machine.

turing(Tape0, Tape) :-
  perform(q0, [], Ls, Tape0, Rs),
  reverse(Ls, Ls1),
  append(Ls1, Rs, Tape).

perform(qf, Ls, Ls, Rs, Rs) :- !.
perform(Q0, Ls0, Ls, Rs0, Rs) :-
  symbol(Rs0, Sym, RsRest),
  once(rule(Q0, Sym, Q1, NewSym, Action)),
  action(Action, Ls0, Ls1, [NewSym|RsRest], Rs1),
  perform(Q1, Ls1, Ls, Rs1, Rs).

symbol([], b, []).
symbol([Sym|Rs], Sym, Rs).

action(left, Ls0, Ls, Rs0, Rs) :- left(Ls0, Ls, Rs0, Rs).
action(stay, Ls, Ls, Rs, Rs).
action(right, Ls0, [Sym|Ls0], [Sym|Rs], Rs).

left([], [], Rs0, [b|Rs0]).
left([L|Ls], Ls, Rs, [L|Rs]).

% writing prolog you don't need to care about input and output, because return
% value can be used as input to search for parameter.

% Q × Γ → Q × Γ × {L, R}
% This is defined as relation between states.
% one state before change and one after.

rule_b21(q0, 1, q0, 1, right).
rule_b21(q0, b, qf, 1, stay).

rule_unary_add(q0, 1, q0, 1, right).
rule_unary_add(q0, +, q1, 1, right).
rule_unary_add(q1, 1, q1, 1, right).
rule_unary_add(q1, _, q2, _, left).
rule_unary_add(q2, 1, q3, _, left).
rule_unary_add(q3, 1, q3, 1, left).
rule_unary_add(q3, _, qf, _, right).

rule(A, B, C, D, E) :- rule_unary_add(A, B, C, D, E).

% a turing machine can be modeled as a conjunction of horn clauses.

%%
%
%% Hore clause pros:
%  1. simple structure, allows fast inference algorithms.
%  2. general enough for all computations.
