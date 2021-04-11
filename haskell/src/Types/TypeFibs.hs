{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}
module TypeFibs where

-- some basics about proofing math with haskell.

-- peano natrual numbers.
data Nat = Z | S Nat
type family Add (a :: Nat) (b :: Nat) :: Nat
type instance Add Z b = b
type instance Add (S a) b = S (Add a b)


type family Fibonacci (n :: Nat) :: Nat
type instance Fibonacci Z = Z
type instance Fibonacci (S Z) = (S Z)
type instance Fibonacci (S (S n)) = Add (Fibonacci n) (Fibonacci (S n))

