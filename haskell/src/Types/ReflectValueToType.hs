{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE PolyKinds              #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeApplications       #-}
{-# LANGUAGE TypeFamilies           #-}

{-# LANGUAGE FlexibleContexts       #-}
module Types.ReflectValueToType where

import           Data.Kind
import           Data.Proxy

-- http://okmij.org/ftp/Haskell/tr-15-04.pdf

-- the point is to manage configuration better (configuration problem)

-- Goal:
--  having part of the code change based on the global configuration.

-- We don't want
--  1. mutable global variable, because it's fp (dogma but take it for now)
--  2. Reader Monad, because now all computations need to be in monad.
--  3. dynamic scoped varaiable like lisp. because it's dynamic scoped...
--  4. No explicitly passing config to all functions because that's silly.

-- An example program that needs configuration: Modulo arithmetics.
--  All binops need to know what modulo it's currently working on.
--  +/Z3 can't be mixed with +/Z5

----- threaded modulus --------------------------------------------------------
-- this makes sure all modulo passed are correct.

newtype Modulus1 s a = Modulus1 a deriving (Eq, Show)
newtype M s a = M a deriving (Eq, Show)

add :: Integral a => Modulus1 s a -> M s a -> M s a -> M s a
add (Modulus1 m) (M a) (M b) = M $ (a + b) `mod` m

mul :: Integral a => Modulus1 s a -> M s a -> M s a -> M s a
mul (Modulus1 m) (M a) (M b) = M $ (a * b) `mod` m

unM :: M s a -> a
unM (M m) =  m

-- straight style
data AnyModulus1 a where
  AnyModulus1 :: Modulus1 s a -> AnyModulus1 a

makeModulus1 :: a -> AnyModulus1 a  -- exitential quantify it.
makeModulus1 a = AnyModulus1 (Modulus1 a)

test1 :: Integer
test1 = case makeModulus1 4 of
          AnyModulus1 m ->
            let a = M 3
                b = M 5
             in unM $ add m (mul m a a) (mul m b b)

-- cps form. We don't even need to introduce a new exitential type wrapper.
-- just pass the threaded to a continuation and all computation is done
-- within.
withModulus1 :: a -> (forall s. Modulus1 s a -> w) -> w
withModulus1 m k = k (Modulus1 m)

test2 :: Integer
test2 = withModulus1 4 $ \m ->
  let a = M 3
      b = M 4
   in unM $ add m (mul m a a) (mul m b b)

-- you need to get conformatble with making all sorts of
-- exitential wrappers. Or different cps techniques.
-- A functon takes a contination can be thought as some qualification over a
-- piece of computation.
test3 :: Integer
test3 = withModulus1 44 $ \m ->  -- has it's unique s
        withModulus1 33 $ \m' ->
          let a = M 3
              b = M 4
           in unM $ add m (mul m a a) (mul m b b)   -- m can't be m'


----- type class for modulus passing
-- avoid explicity passing modulus

-- Modular is actually a function from type s to a value a.
-- Functional dependency enforces one s give one a, so at therm level we can
-- have a unique result.

class Modular s a | s -> a where     -- map type s to a
  modulus :: forall s . a

-- example of a specific instance for modular typeclass
-- if we know s statically we can just know what value modulus should be.
data Label_S_3
data Label_S_4
data Label_S_5

instance Modular Label_S_3 Int where modulus = 3
instance Modular Label_S_4 Int where modulus = 4
instance Modular Label_S_5 Int where modulus = 5

-- >>> modulus @Label_S_3
-- >>> modulus @Label_S_4
-- >>> modulus @Label_S_5
-- 3
-- 4
-- 5

normalize :: forall s a . (Modular s a, Integral a) => a -> M s a
normalize a = M $ a `mod` (modulus @s)

-- normalize propagates type info. Here we're saying: in context of Label_S_3
-- and a being Int, 10 is equivalent to 1
-- >>> normalize @Label_S_3 @Int 10
-- >>> normalize @Label_S_3 @Int 9
-- >>> normalize @Label_S_3 @Int 8
-- M 1
-- M 0
-- M 2

instance (Modular s a, Integral a) => Num (M s a) where
  M a + M b = normalize (a + b)
  M a * M b = normalize (a * b)
  M a - M b = normalize (a - b)
  negate (M a) = normalize (negate a)
  fromInteger i = normalize (fromInteger i)
  signum = error "is mod"
  abs = error "is mod"


-- stucks here.
-- withModulus2 :: a -> (forall s. Modular s a => s -> w) -> w
-- withModulus2 m k = undefined

--  what we want is to pass 3, 3 choose the instance Label_S_3 (we need to
--  somehow generate it), then work with the same instance afterwards
-- test4 :: M Label_S_3 Int
-- test4 = withModulus2 3 $ \_ -> 3 + 4


----- reflect nat to value ----------------------------------------------------

data Zero'
data Suc' s
data Pred' s
data Twice' s

-- ReflectNum reflect a Nat type to value.
-- It uses allow ambiguous types because reflectNum has no parameter, so we
-- must annotate the type to let ghc knows what value to have.
-- It's essentially a function that takes two type parameters s and a and
-- reutrn a value of Num a.
-- To use the functoin, we apply the function with types using "TypeApplication"
-- so reflect @Zero'.
-- This essentially project Zero' on type level to 0 on term level.
class ReflectNum s where
  reflectNum :: forall s a . Num a => a

instance ReflectNum Zero' where
  reflectNum = 0

-- inductive definition
-- How to think about type level programming?
-- A type class is a function from type to term. At type level it's just a
-- type predicate, but it associates some term level definitions.
--
-- If we view it as a funtion definition, each instance is a pattern matching
-- on paramter (a type), the instance head is the guard clause in function.
-- So the instance ReflectNum might as well written as
--
--  ReflectNum (Suc' n) | IsReflectNum n = ...
--  If you see it this way everything falls into the same framework.
instance ReflectNum n => ReflectNum (Suc' n) where
  reflectNum = 1 + (reflectNum @n)

instance ReflectNum n => ReflectNum (Pred' n) where
  reflectNum = (reflectNum @n) - 1

instance ReflectNum n => ReflectNum (Twice' n) where
  reflectNum = (reflectNum @n) * 2

-- >>> reflectNum @(Suc' (Suc' (Suc' (Suc' Zero'))))
-- 4

reifyIntegral :: forall a w . Integral a
              => a
              -- In this case we must use a proxy instead of type application
              -- becasue k is a callback,
              -- it's a exitnetial callback, we pass an exitential quantified
              -- varaible s just to use it in the callback to get a value w.
              -> (forall k (s :: k) . ReflectNum s => Proxy s -> w)
              -> w
reifyIntegral j k
  | j == 0 = k (Proxy :: Proxy Zero')
  | otherwise = reifyIntegral (j - 1)
              $ \(_ :: Proxy s) -> k (Proxy :: Proxy (Suc' s))

test5 n = reifyIntegral n $ \(p :: Proxy s) -> reflectNum @s


----- now, how to create instance at runtime ----------------------------------
-- use polymorphic recursion to choose from infinite family of instances

data ModulusNum s a   -- a type tuple correlates s and a.

-- create an instance for the tuple s a.
instance (ReflectNum s, Num a) => Modular (ModulusNum s a) a where
  modulus = reflectNum @s

withIntegralModulus :: forall a w . Integral a
                    => a
                    -> (forall (s :: Type) . Modular s a => Proxy s -> w)
                    -> w
withIntegralModulus i k = reifyIntegral i
                        $ \(p :: Proxy s) -> k (Proxy :: Proxy (ModulusNum s a))


modn n = withIntegralModulus n $ unM . m
  where
    -- must have this type annotation.
    m :: forall s a . (Integral a, Modular s a)
      => Proxy s -> M s a
    m _ = 12 + 39


----- reifying lists --------------------------------------------------------
data Nil
data Cons s ss

