module plfa.p1.Relations where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm; +-identityʳ)

-- # Relations #
-- define relation of less than
--  note: {} means n is implicit.
data _≤_ : ℕ → ℕ → Set where
    z≤n : ∀ {n : ℕ}
        -----------
        → zero ≤ n

    s≤s : ∀ {m n : ℕ}
        → m ≤ n
        --------------
        → suc m ≤ suc n

infix 4 _≤_

-- ! indexed datatype
-- e.g type m ≤ n is indexed by {m n : ℕ}

-- definition:
--      base case: ∀ n ∈ ℕ, zero ≤ n holds
--                 (z≤n gives he evidence that it holds)
--      inductive case: ∀ m, n ∈ ℕ, m ≤ n → suc m  ≤ suc n
--                      (s≤s takes evidence that m ≤ n holds
--                       and then gives evidence that suc m  ≤ suc n holds)

-- # Implicit arguments #
-- implict arguments need not to be written explicitly. They are
-- inferred by the typechecker.
-- 0 ≤ 2 → 1 ≤ 3 → 2 ≤ 4
_ : 2 ≤ 4
_ = s≤s (s≤s z≤n)
-- write the same proof explicitly
_ : 2 ≤ 4
_ = s≤s {1} {3} (s≤s {0} {2} (z≤n {2}))
-- even more explicitly
_ : 2 ≤ 4
_ = s≤s {m = 1} {n = 3} (s≤s {m = 0} {n = 2} (z≤n {n = 2}))

-- # Inversion #
inv-s≤s : ∀ {m n : ℕ}
    → suc m ≤ suc n
    ---------------
    → m ≤ n
inv-s≤s (s≤s m≤n) = m≤n

inv-z≤n : ∀ {m : ℕ}
    → m ≤ zero
    -----------
    → m ≡ zero
inv-z≤n z≤n = refl

-- # Properties of ordering relations #
--     reflexive:       ∀ n. the relation nRn holds
--     transitive:      ∀ m, n. p, (mRn ∧ nRp → mRp)
--     anti-symmetric:  ∀ m, n. mRn ∧ nRm → m ≡ n
--     total:           ∀ m, n. mRn ∨ nRm
-- ! some name for combination of common properties
--     preorder: reflexive and transitive
--     partial order: any preorder that is also anti-symmetric.
--     total order: any parital order that is also total.

-- exercise orderings
-- An example of a preorder that is not a partial order
--      ans: we want something that is reflexive and transitive, but
--           mRn ∧ mRn doesn implies m ≡ n
--           From wiki:
--              The reachability relationship in any directed graph.
-- An example of a partial order that is not a total order
--      ans: ⊆

-- ! Reflexivity
≤-refl : ∀ {n : ℕ}
    ------
    → n ≤ n
≤-refl {zero} = z≤n
≤-refl {suc n} = s≤s ≤-refl

-- ! Transitivity
≤-trans : ∀ {m n p : ℕ}
    → m ≤ n
    → n ≤ p
    ---------
    → m ≤ p
≤-trans z≤n _ = z≤n
≤-trans (s≤s m≤n) (s≤s n≤p) = s≤s (≤-trans m≤n n≤p)


