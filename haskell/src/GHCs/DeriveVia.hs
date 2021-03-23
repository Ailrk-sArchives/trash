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
-- essentially we are saying they way that Pretty instance works for
-- G is exactly the same as ShowPretty G instance works .
data G = MkT3 Int | MkT4 Bool
  deriving (Show)
  deriving (Pretty) via X G

newtype L = MkT5 Double
  deriving (Show)
  deriving (Pretty) via X L

{-@ First just let's see how is derive via is really used @-}

{-@ Equality constraint and Coercible constraint
    To understand derive via, first you need to now how
    coerce works. (coercible constraint)
    But to understand coercible constraint works, you need to know
    how equality constraint works...

    First, type context is the current environment, all our types are
    in the environment. You can think it as a set S
    let's say forall t1, t2 \in S,  t1 ~ t2
    This means t1, t2 need to be the same
@-}

-- {
-- use functional dependencies to add unique dependency constraint.
-- 1.
class C' a b | a -> b

-- use equality constraint to say a map of a with F should be the same
-- as b, which means a functional dependency.
-- 2.
class (F a ~ b) => C a b where
  type F a -- saying there exist F s.t F a ~ b, (a determine b uniquely)
  -- }

{-@ Coerce
    The coerce function in Data.Coerce has nothing to do with
    C++ style coercion, it only does safe coerce.
    Namely coerce :: Coerciable a b => a -> b.
    Thus to be able to coerce a to b, there must exist a instance
    Coercible instance between a and b.

 @-}
