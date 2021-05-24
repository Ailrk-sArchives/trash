-- Extension started with [x] means it's already adopted in GHC2021, which means
-- it's stable to use and works well with the rest of haskell type system.
-- A lot of extensions are just necessary to write haskell today, the reason they are
-- extension is just because it's the way haskell adding new features.
-- At some point when the number of extensions exploded the committee will do something
-- to offically merge common features into GHC.

-- Promotes term level values into types, and type level values into kinds.
{-# LANGUAGE DataKinds                #-}

-- [x] Allows you to give different names for kinds instead of * only.
{-# LANGUAGE PolyKinds                #-}

-- [x] Allows you to pass polymorphic functions around.
{-# LANGUAGE RankNTypes               #-}

-- [x] Specify type variable based on constructors.
--     also a better syntax for exitential types.
{-# LANGUAGE GADTs                    #-}

-- Allow type variable to be referred under the scope of where it's declared.
{-# LANGUAGE ScopedTypeVariables      #-}

-- Allow you to write the kind signature of a single type.
{-# LANGUAGE KindSignatures           #-}

-- [x] Allow you to write the full kind signature of a type for a type declaration.
{-# LANGUAGE StandaloneKindSignatures #-}

-- [x] Allows you to specify the type of typeclass constriant by passing a type as
--     a extra paramter. The type parameter is written as @Type. --
--     visible type application --
{-# LANGUAGE TypeApplications         #-}

-- Type level function.
{-# LANGUAGE TypeFamilies             #-}

-- [x] Allows type level operators like ':
{-# LANGUAGE TypeOperators            #-}

-- Instance declaration must follows some rules to ensure it terminates. If we remove the
-- restriction to force a declaration should always terminate, we have this extension.
-- might cause non terminated type checking --
--
-- So the problem is in what case a instance is undicidable?
--  1. Paterson condition
--  2. Coverage condition
-- The core is still induction. You want to make sure each reduction step makes the problem
-- smaller by at least on constructor so we can guarantee it terminates.
{-# LANGUAGE UndecidableInstances     #-}

-- [x] Allow the use of complex constraints in class declaration.
--     Context might mean typeclass constraints on type variables of the class or of class
--     functions.
--     With this extension on, the only restriction on class hierarchy is it should be
--     acyclic (acyclic only on the superclass relation).
{-# LANGUAGE FlexibleContexts         #-}

-- [x] Allow definition of type class instances with arbitrary nested types in the instance head.
--     With this extension you can mention concrete type in the instance head.
{-# LANGUAGE FlexibleInstances        #-}

{-# LANGUAGE TypeSynonymInstances     #-}

{-# LANGUAGE AllowAmbiguousTypes      #-}

-- Allow you to define typeclasses based on multiple paramters.
-- it's an example of multi dispatch in haskell.
{-# LANGUAGE MultiParamTypeClasses    #-}

{-# LANGUAGE InstanceSigs #-}
module Types.Kinds where

import           Data.Kind
import           Data.Proxy
import           GHC.Generics
import           GHC.TypeLits

import           Control.Monad.Trans

-- allowed by FlexibleContexts
class A cls c
class A B c => B c

-- allowed by FlexibleInstances
data A' a
class C' m
instance C' (A' Int)

-- allowed by TypeSynonymInstances + FlexibleInstances
type Point = (Int, Int)
instance C' Point

type Vector3 a = (Int, Int, Int, a)
instance (Monoid (m a), Monad m) => C' (Vector3 (m a))

-- normal higher kinded type paramter.
class C'' (m :: * -> *)
instance C'' A'


{-@ 1. closed type families @-}
-- some type level programing.
-- standalone kind signature / kind signature + type familiy + polykind to
-- write type level function as if it's normal function.
-- data kinds promote all term level values into type, so we can do all term level
-- computations at the type level.

-- this form is called a closed type family.
type Append :: forall a. [a] -> [a] -> [a]
type family Append xs ys where
  Append '[] ys = ys
  Append (x ': xs) ys = x ': Append xs ys

-- '[1, 2, 3, 4, 5, 6]
type ListType1 = Append '[1, 2, 3] '[4, 5, 6]

type Cons :: forall a. a -> [a] -> [a]
type family Cons a xs where
  Cons a '[] = '[a]
  Cons a xs = a ': xs

-- --
type Consed1 = Cons 1 '[1, 2, 3]
type ConsedList = Append (Cons 1 (Append '[1, 2, 3] '[3, 4, 5])) ListType1
-- --

type Take :: forall a. Nat -> [a] -> [a]
type family Take n xs where
  Take 0 xs = '[]
  Take n (x ': xs) = x ': Take (n - 1) xs

type Length :: forall a. [a] -> Nat
type family Length xs where
  Length '[] = 0
  Length (x ': xs) = 1 + (Length xs)

-- --
type Take3OnList = Take 3 ConsedList
type LengthOfList = Length ConsedList
type Take30OnList = Take 30 ConsedList
-- --


{-@ 2. open type families @-}
type Label :: Type -> Symbol   -- type level string
type family Label t where
  Label Double = "number"
  Label String = "string"

-- how do we get the label into term level?
label :: forall t. KnownSymbol (Label t) => String
label = symbolVal (Proxy @(Label t))

v1 = label @Double
v2 = label @String


{-@ 3. @-}
data Currency
  = USD
  | CND
  | JPY
  | RMB
  | EUR
  deriving (Show, Eq)


type family IsAmericanDollar (a :: Currency) where
  IsAmericanDollar 'USD = ()
  IsAmericanDollar a = TypeError ('Text "It's not American dollar")

type R1 = IsAmericanDollar 'USD
type R2 = IsAmericanDollar 'RMB
