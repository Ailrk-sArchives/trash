{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DerivingStrategies    #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE KindSignatures        #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE TypeOperators #-}

module Types.DependentType where

{-@ Four capabilities of dependent type systems.
    1. Type camputation
        - program the type checker.
    2. Indexed types
        - type indices constrain values and guide
          computation
    3. Double-duty data
        - using both data in the type indicies and runtime
        - quantify a type parameter with PI, and you can
          get the type in the term level. This is what dependent
          type brings.
        - singleton is used to simulate pi type.
    4. Equivalcne proof
@-}

-- A super short primer.

{-@ Dependent vector with vector length in type.
@-}

-- Do we need this? Nat is already a kind in haskell I think
data Nat = Zero | Succ Nat deriving stock (Eq, Show)


{-@ Now define the vector
    Vector :: Nat -> * -> *. The Nat part is where we encode the length.
    The syntax (n :: Nat) is enabled by KindSignatures.
    Note we are using the promoted data constructor as types now.
@-}


data Vector (n :: Nat) a where
  Nil :: Vector Zero a
  Cons :: a -> Vector n a -> Vector (Succ n) a

instance Show a => Show (Vector n a) where
  show Nil         = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ "(" ++ show xs ++ ")"

{-@ now some type level operations.
    how do you express m + n in the following vector type?

    We don't know yet. With PartialTypeSignatures, I can put a _hold there
    to indicate it could be anything.
@-}

append' :: Vector n a -> Vector m a -> Vector ((_ :: Nat) :: Nat) a
append' = undefined

type family Add n m where
  Add 'Zero m = m
  Add ('Succ n) m = 'Succ (Add n m)

{-@ Very sadly, if you change the order of m and n, or change the order
    of succ in the above type family, you get type error.
    GHC cann't infer if two types are the same...
    And apparently this is the next step people working towards.
@-}

append :: Vector n a -> Vector m a -> Vector (Add n m) a
append Nil ys         = ys
append (Cons x xs) ys = Cons x (append xs ys)

{-@ Ok that's lame. But let's do something else.
    You can use symbols like normal haskell list syntax at type level with
    TypeOperators
@-}

data HList xs where
  HNil :: HList '[]
  (:::) :: a -> HList as

infix 6 :::
