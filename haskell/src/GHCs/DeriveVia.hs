{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module GHCs.DeriveVia where

import Data.Coerce

-- --------------------------------------------------------------------------
-- heat up
-- --------------------------------------------------------------------------

-- first rule: we can't derive things witout an implementation of course. no magic
-- second rule: we can derive things based on default implementation
class Pretty a where
  ppr :: a -> String
  default ppr :: (Show a) => a -> String
  ppr = show

data T = MkT1 Int | MkT2 Bool deriving (Show)

instance Pretty T

-- --------------------------------------------------------------------------
-- use derive via.
-- --------------------------------------------------------------------------

-- first, define a wrapper newtype
-- we can parameterized this X.
newtype X a = X a

instance Show a => Pretty (X a) where
  ppr (X x) = "Pretty" ++ show x

-- we use the shape of ShowPretty to define the instance Pretty for T
--
-- essentially we are saying:
--  how Pretty instance works for G is the same as how ShowPretty G instance works .
data G = MkT3 Int | MkT4 Bool
  deriving (Show)
  deriving (Pretty) via X G

newtype L = MkT5 Double
  deriving (Show)
  deriving (Pretty) via X L

{-@ First just let's see how is derive via is really used @-}

{-@ Equality constraint and Coercible constraint

    before talk about Dirive via,

    - type context: current enviroment (includes typeclass etc.)
      a set S with all other types we can access. It's Γ.

    - what's equality constraint ?
      ∀ t1, t2 ∈ S, t1 ~ t2

    - what's coerce? (coercible constraint)
      t1 ~ t2 => t1 coerciable to t2 in a type safe way.
@-}

-- side:
--
-- Multi parameter type class + functional dependencies VS type family + equality constraint
-- for expression a determine an unique b:
class C' a b | a -> b
class (F a ~ b) => C a b where
  type F a

{-@ Coerce
    The coerce function in Data.Coerce has nothing to do with
    C++ style coercion, it only does safe coerce.

    coerce :: Coerciable a b => a -> b.

    Thus to be able to coerce a to b, there must exist a instance
    Coercible instance between a and b.
 @-}
