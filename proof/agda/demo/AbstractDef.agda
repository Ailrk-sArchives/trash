module demo.AbstractDef where

import Relation.Binary.PropositionalEquality as Eq
-- open the module (add names specified in using)
open Eq using (_≡_; refl; cong; sym)
open Eq.≡-Reasoning using (begin_; _≡⟨⟩_; _∎)
open import Data.Nat using (ℕ; zero; suc; _+_; _*_; _∸_)
open import Function using (_∘_; flip)


-- abstract definitions

-- define integer as the difference of two natural numbers
abstract
  ℤ = ℕ × ℕ

  0ℤ : ℤ
  0ℤ = 0 , 0

  1ℤ : ℤ
  1ℤ = 1 , 0

  _+ℤ_ : (x y : ℤ) → ℤ
  (p, n) +ℤ (p' , n') = (p + p') , (n + n')

  -ℤ_  : ℤ → ℤ
  -ℤ (p , n) = (n , p)

  _≡ℤ_ : (x y : ℤ) → Set
  (p, n) ≡ℤ (p' , n') = (p + n') ≡ (p' + n)

  private
    postulate
      +comm : ∀ n m → (n + m) ≡ (m + n)

  invℤ : ∀ x → (x +ℤ (-ℤ x)) ≡ℤ 0ℤ
  invℤ (p, n) rewrite +comm (p + n) 0 | +comm p n = refl
