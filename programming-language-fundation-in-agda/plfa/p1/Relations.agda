module plfa.p1.Relations where

import Relation.Binary.PropositionalEquality as Eq
open Eq using (_≡_; refl; cong)
open import Data.Nat using (ℕ; zero; suc; _+_)
open import Data.Nat.Properties using (+-comm; +-identityʳ)

-- # Relations #
data _≤_ : ℕ → ℕ → Set where
    z ≤ n : ∀ {n : N}
    -------------------
    → zero ≤ n

    s ≤ n : ∀ {m n : N}
        → n ≤ n
        -------------------
        → suc m ≤ suc n

