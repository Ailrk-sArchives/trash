module plfa.p1.Induction where

-- ## Prove properties of inductive type by induction

import Relation.Binary.PropositionalEquality as Eq
-- open the module (add names specified in using)
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _≡⟨_⟩_; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)

-- # Properties of operators

-- prove associativity
-- base case + inductive step
-- type sig is the proposition
-- the function definition is the evidence we provide as the proof.
+-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p)
+-assoc zero n p =
    begin
        (zero + n) + p
    ≡⟨⟩
        n + p
    ≡⟨⟩
        zero + (n + p)
    ∎

+-assoc (suc m) n p =
    begin
        (suc m + n) + p
    ≡⟨⟩
        suc (m + n) + p
    ≡⟨⟩
        suc ((m + n) + p)
    ≡⟨ cong suc (+-assoc m n p) ⟩  -- provide justification. use the induction hyp
        suc (m + (n + p))
    ≡⟨⟩
        suc m + (n + p)
    ∎

-- cong stands for congruence.
-- a relation is said to be congruence for a given function if it preserved by
-- applying that function. If e is evidence that x ≡ y, then cong f e is
-- evidence that f x ≡ f y ∀ f.

-- ! Dependent function
-- +-assoc : ∀ (m n p : ℕ) → (m + n) + p ≡ m + (n + p) has different type
-- compare  with that of a traditional function say ℕ → ℕ → ℕ.
-- Variables are associated with each argument type, and the result type
-- may depend upon these variables. Hence they are called dependent function.

-- ! proof commutativity
-- Proof it by proving several smaller lemma
-- base case of addition state zero is left-identity

-- Lemma 1: proof zero is also right-identity
+-identityτ : ∀ (m : ℕ) → m + zero ≡ m  -- <- proposition (type)
+-identityτ zero =                      -- <- pf (function take m return evidence)
    begin
        zero + zero
    ≡⟨⟩
        zero
    ∎

+-identityτ (suc m) =
    begin
        (suc m) + zero
    ≡⟨⟩
        suc (m + zero)
    ≡⟨ cong suc (+-identityτ m) ⟩
        suc m
    ∎

-- lemma 2: proof the other direction of inductive case of addition.
+-suc : ∀ (m n : ℕ) → m + suc n ≡ suc (m + n)
+-suc zero n =
    begin
        zero + suc n
    ≡⟨⟩
        suc n
    ≡⟨⟩
        suc (zero + n)
    ∎

+-suc (suc m) n =
    begin
        suc m + suc n
    ≡⟨⟩
        suc (m + suc n)
    ≡⟨ cong suc (+-suc m n) ⟩
        suc (suc (m + n))
    ≡⟨⟩
        suc (suc m + n)
    ∎

-- finally proof commutativity
+-comm : ∀ (m n : ℕ) → m + n ≡ n + m
+-comm m zero =
    begin
        m + zero
    ≡⟨ +-identity m ⟩
        m
    ≡⟨⟩
        zero + m
    ∎

+-comm m (suc n) =
    begin
        m + (suc n)
    ≡⟨ +-suc m n ⟩
        suc (m + n)
    ≡⟨ cong suc (+-comm m n) ⟩
        suc (n + m)
    ≡⟨⟩
        suc n + m
    ∎

-- # rearranging corollary #
-- proof the first corollary


