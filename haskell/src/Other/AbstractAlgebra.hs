{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell       #-}
module Other.AbstractAlgebra where


import           Prelude (Bool, Eq, (==))


-- use it to review some abstract algebra.

{-@  First we have semigroup @-}
-- A semigroup is a set with a bianry associative operation
--
-- in haskell you can view it as declaring a semigroup with
-- elements in set a. a is polymorphic, you can specialize it
-- to any types as long as you define the binary operator.
--
-- Semigroup has no rules, it's whole point is to better define
-- associativity. (Now you have an entire algebraic strucuture
-- with associativity only)
--
-- - Associativity
-- Associativity implies the order of assoication doesn't matter.
-- With this rule in hand we hand group arbitrary elements together
-- and evaluate them in different order.
--
-- But one thing to notice is you don't guarantee commutivity in
-- a semigroup.
--
-- We say a <> b a mutiply b, and the result is their product. even
-- though now it as nohting to do with real number multiplication.
-- You can also call them sum, or other things, that's the whole
-- point of being abstract.

class Semigroup a where
  (<>) :: a -> a -> a

semigroup_assoc :: (Semigroup a, Eq a) => a -> a -> a -> Bool
semigroup_assoc a b c = ((a <> b) <> c) == (a <> (b <> c))


{-@ On top of that we have monoid @-}
-- a monoid is a set with a binary operation has associativity and identity.
-- A monoid must first be a semigroup, so you get associativity for free.
-- Identity means there exists an element such that anything in the monoid
-- applies with it you get the element back.
--
-- Identity is the most special elemnet in an algebra.

class (Semigroup a) => Monoid a where
  mempty :: a

-- you also have left id and right id, don't show here.
monoid_id :: (Monoid a, Eq a) => a -> Bool
monoid_id a = (mempty <> a) == a


{-@ On top of that we have group @-}

-- All elements in a group can have an inverse.
-- And just these three properties so far, we openned the gate to
-- the entire group theory.
class (Monoid a) => Group a where
  inverse :: a -> a


{-@ Group homomorphism! @-}
-- Some groups can be similar to the other, and some of them are
-- exactly the same.
-- When we define homomorphism, we define it as
-- f : A -> B
-- with the property f(xy) = f(x)f(y)
-- What this implies is the mapping doesn't touch the underlying
-- algebraic structure. After all the interaction between elements
-- form an algebra. And if you can map an element from one group
-- to another without their algebraic properties, this mapping
-- must be somehitng interesting.

-- In haskell we can have multiparameter typeclass to model
-- the relationship between two types.
-- This gives you much more freedom then just map an element to
-- another, but homomorphism only requires this.

group_inverse :: (Group a, Eq a) => a -> Bool
group_inverse a = (a <> inverse a) == mempty

-- elemnents of a group form a set. We have either finite group or
-- infinite group, just like set.

-- We have this very abstract notion here, but what are some examples of
-- a group?

-- First lets define some finite set.
-- Because we don't have integer, let's use enum to represent them.
-- Introducing integer modulo N. a + b mode n
data Z3 = Z3_0 | Z3_1 | Z3_2

-- here we can define all the possible combination of elements in the
-- group. This effectively give us the multiplication table of Z3.
instance Semigroup Z3 where
  Z3_0 <> a    = a
  a <> Z3_0    = a
  Z3_1 <> Z3_1 = Z3_2
  Z3_2 <> Z3_2 = Z3_1
  Z3_1 <> Z3_2 = Z3_0
  Z3_2 <> Z3_1 = Z3_0

-- you merely give identity a name here. Operation of idenity is
-- still defined on Semigroup.
instance Monoid Z3 where
  mempty = Z3_0

-- to be a group we need inverse.
instance Group Z3 where
  inverse Z3_0 = Z3_0
  inverse Z3_1 = Z3_1
  inverse Z3_2 = Z3_2

-- There we defined a complete group. It' tedious so later we will generate
-- it with template hasekll.

-- Look at the group, you find, we not only have associativity, if I pick
-- 1 <> 2 = 0, 2 <> 0, it seems we also have commutivity.
-- a group with commutitive binary opeartor is called an abelian group.

class (Group a) => AbelianGroup a where


-- TODO
-- further more, this is a cyclic group

class (AbelianGroup a) => CyclicGroup a where
  generator :: [a]


class (Group a, Group b) => Homomorphism a b where
  homomorphism :: a -> b

-- Looking at a map from a to b, you might wonder if there is an
-- inverse to it.
-- Notice the homomorphism we define is only one way, a to b. In
-- general, homomorphism doesn't guarantee has an inverse.
