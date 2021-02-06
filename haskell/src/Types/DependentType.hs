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

{-@ Dependent vector
    Let's make everyone's favorite dependent vactor with it's length
    encoded in it's type.
@-}

{-@ DataKinds promotes value to type, type to kind.
    For the peano number below, We get a new kind `Nat` and two new
    types `'Zero :: Nat` and `'Succ Nat :: Nat -> Nat`
    Note, Nat :: *, It's the constructors get the kind Nat.
@-}
data Nat = Zero | Succ Nat deriving stock (Eq, Show)

{-@ Now let's define the vector
    Note Vector :: Nat -> * -> *. The Nat part is where we encode the length.
    The syntax (n :: Nat) is enabled by KindSignatures.

    Note we are using the promoted data constructor as types now.
@-}

data Vector (n :: Nat) a where
  Nil :: Vector Zero a
  Cons :: a -> Vector n a -> Vector (Succ n) a

instance Show a => Show (Vector n a) where
  show Nil         = "Nil"
  show (Cons x xs) = "Cons " ++ show x ++ "(" ++ show xs ++ ")"

{-@ Now you construct interesting types, you might also want
    some type level operations.
    Like, how do you express m + n in the following vector type?
    We don't know yet. With PartialTypeSignatures, I pout a _ there
    to indicate it could be anything.
@-}
append' :: Vector n a -> Vector m a -> Vector ((_ :: Nat) :: Nat) a
append' = undefined

{-@ You need type families
    Let's define a type family Add takes two Nats n, m as parameter and
    create a new type.

    Note, for this to work you need UndecidableInstances, because GHC can't
    figure out if the type family you write is decidable or not.
@-}

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
