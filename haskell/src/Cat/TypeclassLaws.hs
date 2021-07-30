module Cat.TypeclassLaws where

import Test.Hspec

-- laws on typeclass specifies the property certain operation should have. It
-- cannot be enforced by haskell's type system, but it's important because it
-- affect the semantics of typeclasses.


------------------------------------------------------------------------------
-- Functor Law:

class Functor' f where
  fmap' :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
  (<$) = fmap' . const


-- 1. identity:
--      fmap id = id
-- 2. homomorphic:
--      fmap (g . h) = (fmap g) . (fmap h)
--
-- These two laws enforecs functors to map while preserves the structure.
-- A thing to note is that (f a) and (f b) are essentially different types,
-- just they share the common structure f.
--
-- So the most natural map from (f a) to (f b) should not change the structrure
-- of f.
--
-- e.g
--  1. of course this holds
--     fmap (id . id) = fmap id . fmap id
--
--  2. fmap (const "a") (Just 1) = Just "a"
--     The structure of Maybe is not chagned, but all values from Int
--     maps to one value "a".
--
--  3. fmap fmaps between types with the same structure. You don't map from
--     Maybe Int to [Int].
--     An arbitrary map between functors requires us to handle in a case by case
--     manner.

------------------------------------------------------------------------------
-- Applicative Law:

class Functor' f => Applicative' f where
  pure :: a -> f a
  infixl 4 <*>, *>, <*

  (<*>) :: f (a -> b) -> f a -> f b
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a

-- Laws
-- 1. identity
--      pure id <*> v = v
-- 2. homomorphic
--      pure f <*> pure x = pure (f x)
-- 3. interchange
--      u <*> pure y = pure ($ y) <*> u
-- 4. u <*> (v <*> w) = pure (>) <*> u <*> v <*> w

-- all applicatives should also be a funtor. that means we can always count on
-- simple structure preserving mapping. The reason why we can claim this is
-- because we can easily define fmap with (<*>) and pure.
-- given a function f :: a -> b we just to do \f m -> (pure f) <*> m to get fmap
--
-- In another word, stronger abstractions provides more facilities that allows
-- us to simulate simpler one.
--
--

