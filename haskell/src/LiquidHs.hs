{-# LANGUAGE OverloadedStrings #-}

module LiquidHs where

import           Prelude hiding (head, length, tail)

-- Abstract
--  1. Refinement types: Types + Predicates.
--  2. Specify properties: From refined input/outpt types.
--  3. Verify properties: Va SMT based predicate subtyping.

-- Natrual numbers
{-@ type Nat = {v:Int | 0 <= v} @-}
{-@ type Pos = {v:Int | 0 <= v} @-}

{-@ nats :: [Nat] @-}
nats = [0, 1, 2, 3]

{-@ poss :: [Pos] @-}
poss = [1, 2, 3, 4]


-- A term with many types:
{-@ zero :: Zero @-}
zero = 0
{-@ zero' :: Nat @-}
zero' = zero

-- Predicate subtyping
-- In environment Γ, t₁ is subtype of t2
-- Γ ⊢ t₁ ⪯ t₂
--
-- Where environment is a sequence of binders.
--     ----
-- Γ ≐ xi:Pi
--
-- If VC is valid ∧ᵢPᵢ ⇒ Q ⇒ R
-- then xi:Pi ⊢ {v:b | Q} ⪯  {v:b | R}

{-@ four :: Nat @-}
four = x + 1
  where
    x = 3


-- Contracts: function types
-- no value satisfies false ⇒ no valid inputs for impossible.
-- ⇒ impossible will never be called at runtime.
{-@ impossible :: {v:_ | false } -> a @-}
impossible msg = error msg

-- precondition
-- This is how you write normal safe dev.
safeDivBad :: Int -> Int -> Int
safeDivBad _ 0 = impossible "divide-by-zero"
safeDivBad x n = x `div` n

{-@ type NonZero = {v: Int | v /= 0} @-}  -- 0 is checked now
{-@ safeDiv :: u: Int -> v: NonZero -> Int @-}
safeDiv x n = x `div` n

avg2 x y = safeDiv (x + y) 2
-- with refinement type the precondition is
--    (v = 2) ⇒ (v ≠ 0)
-- ----------------------
--  x:Int, y:Int ⊢ {v|v=2} ⪯ {v|v≠0}
-- Thus safeDiv typecheck.


-- specify post condition
{-@ size :: [a] -> Pos @-}
size [_]    = 1
size (_:xs) = 1 + size xs
size _      = impossible "size"

-- now combine size and safeDiv, the precond and post cond compose.
avg' xs = safeDiv total n
  where
    total = sum xs
    n = size xs


-- Describe properties of structures.
data List a = Emp | (:<:) a (List a)

-- using measure to strengthens type of data constructor
{-@ measure length @-}
length :: List a -> Int
length Emp        = 0
length (_ :<: xs) = 1 + length xs


{-@ head :: List a -> a @-}
head (x :<: _) = x
head _         = impossible "head"


{-@ tail :: List a -> List a @-}
tail (_ :<: xs) = xs
tail _          = impossible "tail"

-- naming nonempty list
{-@ type LisetNE a = {v: List a | 0 < length v} @-}
