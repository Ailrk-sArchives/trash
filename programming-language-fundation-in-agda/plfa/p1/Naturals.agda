-- # Naturals are inductive datatype #
-- definition of natural number in agda
data ℕ : Set where
    zero_ : ℕ
    suc_ : ℕ → ℕ

-- here zero and suc are constructors of the datatype.
-- ! Inductive definition:
--  Base case: zero ∈ ℕ
--  Inductive step: m ∈ ℕ → suc m ∈ ℕ
-- so zero is ∈ ℕ, (suc (suc zero)) is also ∈ ℕ

-- # unpacking the inference rules #
-- inference rule consists:
--      zero or more judgments (hypotheses)
--      -----------------------------------
--      one judgment (conclusion)

-- # use pragma #
--
-- this load N directly use 0, 1 .. as short hand for zero, (succ zero) ..

{-# BUILTIN NATURAL ℕ  #-}


-- # import #
--
import Relation.Binary.PropostionalEquality as Eq   -- brings std equality into the scope
open Eq using (_≡_; refl)                           -- open the module (add names specified in using)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)

-- # Operations on natruals are recursive functions #
-- we use pattern matching when constructors appear on the left hand side of an equation.
_add_ : ℕ -> ℕ -> ℕ
zero add n = n
(suc m) add n = suc (m add n)
-- the definition of addition is recursive, because
--  add is defined based on definition of add.
--  because the inductive definition of natrual number circularity is not a problem.
--  Larger numbers are defined in terms of smaller numbers (such definition is called well founded).

--  dummy case name _ can be reused
_ : 2 add 3 ≡ 5  -- after : is a type.
_ =
    begin
        2 + 3
    ≡⟨⟩     -- is short hand for
        (suc (suc zero)) + (suc (suc (suc zero)))
    ≡⟨⟩     -- inductive case (associativity)
        suc ((suc zero) + (suc (suc (suc zero))))
    ≡⟨⟩     -- inductive case (associativity agagin)
        suc (suc (zero + (suc (suc (suc zero)))))
    ≡⟨⟩     -- base case
        suc (suc (suc (suc (suc zero))))
    ≡⟨⟩     -- is longhand for
        5
    ∎

-- in compact form
_ : 2 add 3 ≡ 5
_ =
    begin
        2 add 3
    ≡⟨⟩
        suc (1 add 3)
    ≡⟨⟩
        suc (suc (0 add 3))
    ≡⟨⟩
        suc (suc 3)
    ≡⟨⟩
        5
    ∎

-- check reflexive (a binary relation is reflexive if every value relates to itself)
_ : 2 add 3 ≡ 5
_ = refl

-- ! How does agda run these code?
--  Agda check is each term simplifies to the same value.
--  So you can omit some lines and the whole term is still valid. Extra terms are good for readability.

-- ! 2 + 3 ≡ 5 is a type
-- ! chain of equations are terms of the given type
-- ! you can also think it as:
--      type is the proposition
--      term is the evidence
-- ! This duality is central to how we formalize concepts in Agda.

-- Exercise 3 add 4
_ : 3 add 4 ≡ 7
_ =
    begin
        3 add 4
    ≡⟨⟩
        (suc 2) add (suc 3)
    ≡⟨⟩
        suc (2 add (suc 3))
    ≡⟨⟩
        suc ((suc 1) add (suc 3))
    ≡⟨⟩
        suc (suc (1 add (suc 3)))
    ≡⟨⟩
        suc (suc (suc (0 add (suc 3))))
    ≡⟨⟩
        suc (suc (suc (suc 3)))
    ≡⟨⟩
        7

-- # Multiplication #
