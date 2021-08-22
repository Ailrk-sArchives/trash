{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE GADTs                    #-}
{-# LANGUAGE KindSignatures           #-}
{-# LANGUAGE PolyKinds                #-}
{-# LANGUAGE ScopedTypeVariables      #-}
{-# LANGUAGE StandaloneKindSignatures #-}
{-# LANGUAGE TypeFamilies             #-}
{-# LANGUAGE TypeOperators            #-}
module Proof.Nat where

import           Data.Kind
-- import Data.Type.Eq

-- Proof: series of deductive arguments.

infix 4 :~:
data a :~: b where
  Refl :: a :~: a

lemma_sym_'eq :: (a :~: b) -> (b :~: a)
lemma_sym_'eq Refl = Refl

lemma_trans_'eq :: (a :~: b) -> (b :~: c) -> (a :~: c)
lemma_trans_'eq Refl Refl = Refl

data Nat = Z | S Nat deriving Show

type family Add (a :: Nat) (b :: Nat) :: Nat where
  Add Z m = m
  Add (S n) m = S (Add n m)

-- term level refl, if type check the proposition is proofed.
-- then refl becomes the proof.
lemma_0_plus_0_equals_zero_'add :: (Add Z Z) :~: Z
lemma_0_plus_0_equals_zero_'add = Refl

lemma_1_plus_1_equals_two_'add :: (Add (S Z) (S Z)) :~: S (S Z)
lemma_1_plus_1_equals_two_'add = Refl

lemma_left_identity_'add :: (Add Z a) :~: a
lemma_left_identity_'add = Refl

-- TODO. Haven't prove commutativity.
lemma_right_identity_'add :: (Add a Z) :~: a
lemma_right_identity_'add = undefined
