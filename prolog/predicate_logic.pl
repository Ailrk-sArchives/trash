% predicate logic
%
% predicate logics are formulas built from
%   1. terms. e.g f(x, y), s(s(0))
%   2. predicate symbols.  P, Q
%   3. negation
%   4. binary logic connectives (∧ ∨ →)
%   5. quantifiers ∀ ∃

% An interpretatoin of a first order langauges:
%   - a nonempty set D, as the domain of discourse.
%   - a function I, mapping function symbols to functoins Dⁿ → D.
%     andpredicate symbols to relatons over Dⁿ.

%  A formula is `satisfiable` if there is an interpretatoin under which it is
%  true.
%  Such interpretatoin is called a `model`.

% A formula is valid it's true under all interpretatoin.

% If every interpration that makes A true also makes B treu, then
% B is a semantic consequence of A written
%   A |= B.

% e.g  M(c)             "ther predicate M holds for c."
%      ∀x. M(x)         "for all x, M(x) holds"
%      ∀x. P(x) → M(x)  "for all x if x is a person, it's a model"

% Relation
%   Predicate takes more than one arguments express a relation between it's
%   arguments.
%
%   some examples:
%     <(x,y),
%     Parent(x, y)
%     Ancesstor(x, y)
%     =(a, b)
%     Sum(x, y, z)

% Theories
%   A theory is a set of sentences. each sentences are called theorems.
%   Commonly specified as a set of `axioms`.
