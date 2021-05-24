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
module Types.ExtensionsOnTypes where

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
