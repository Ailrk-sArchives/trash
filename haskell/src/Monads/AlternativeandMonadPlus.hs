{-# LANGUAGE AllowAmbiguousTypes #-}

module Monads.AlternativeandMonadPlus where


import           Control.Applicative
import           Control.Monad

-- experiement target
data Option a = Some a | None
data List a = Cons a (List a) | End

--  Some thoughts about.
--  A way to look at it is to override a super generic operator
--  you can shove any type of conceivable computation into it.
--  the only law it needs to hold is align the type of
--  m a -> (a -> m b) -> m b
--
--    1. It's generic, what does (a -> m b) do is totally unlimited.
--    2. It can be chained. since m b is able to be feed into another
--      (b -> m c)
--    3. It can manage a external state. since m can carries a environment
--       through out the chain.
--       Do notation is a sugar for construct such a chain.
--  compare with functor: (a -> b) -> f a -> f b:
--    1. It cannot carry enviroment. The function part (a -> b) can operate
--       on value f holds, but it can not access the context itself.
--    2. it can chain operation. since f b is returned, one can
--       f3 <$> (f2 <$> (f1 <$> x))
--    3. it is also generic. (it's the bottom line when talking about
--       algebraic structure anyway..)
--  compare with applicative f (a -> b) -> f a -> f b
--    1. It can carry environment. f (a -> b) have some implication of
--       f for a -> b to use.
--    2. It can chain
--    3. generic.
--
--   So applicative is also very generic. But why use monad but not applicative?
--
--   Notice one important feature of monad is short circuit failure within the
--   computation.
--   for (>>=) :: m a -> (a -> m b) -> m b, consider m a is the value for the
--   past and m b is the value in the future. For this transition the stepper
--   (a -> m b) must be evaluated. But (a - m b) implies a must exist. For
--   instance, for Either a b, if Either is Left b, a is not present, then the
--   monad chain will short circuit to Left b immediately.
--
--   But for applicative, (<*>) impose no restriction on value of the past.
--   It can combine like Right id <$> Left 1 <*> Left 2<*>, the result could be
--   Left [1, 2] or whatever you defined, but cannot be 1.


-- divide and conquer.
-- a list is [] or _:_,
-- and since _:_ is a list of list it can
-- also be divided as []:_ or _:_
concatList :: List (List a) -> List a
concatList End                    = End
concatList (Cons End xss)         = concatList xss
concatList (Cons (Cons x xs) xss) = Cons x $ concatList (Cons xs xss)

instance Functor Option where
  fmap f (Some a) = Some $ f a
  fmap f None     = None

instance Monad Option where
  return = Some
  (Some a) >>= f = f a
  None >>= f     = None

instance Applicative Option where
  pure = return
  (<*>) = ap

instance Functor List where
  fmap f (Cons a xs) = Cons (f a) (fmap f xs)
  fmap f End         = End

instance Monad List where
  return a = Cons a End
  End >>= f = End
  xs >>= f  = concatList $ fmap f xs

instance Applicative List where
  pure = return
  (<*>) = ap

instance Semigroup (List a) where
  End <> a = a
  a <> End = a
  a <> b   = concatList $ (Cons a (Cons b End))

instance Monoid (List a) where
  mempty = End

-- Alternative captures 'amalgamation' in a general way.
-- empty: the unit
-- <|>:   binary operator combines two computations.

instance Alternative Option where
  empty = None
  None <|> None     = None
  Some x <|> None   = Some x
  None <|> Some x   = Some x
  Some x <|> Some y = Some x

-- use monoid for Alternative direcly.
instance Alternative List where
  empty = End
  (<|>) = (<>)

-- example of parallel parsing.
-- parser libraries use Alternatives a lot to proivdes
-- rich operations to combine different outputs.
digit :: Int -> String -> Option Int
digit _ [] = None
digit i (c : _)
  | i > 9 || i < 0 = None
  | otherwise = if [c] == show i then Some i else None

binChar :: String -> Option Int
binChar s = digit 0 s <|> digit 1 s <|> digit 2 s

-- MonadPlus like a Alternative for Monad.
-- mzero
-- mplus
-- historical legacy. It now conventionally be regard as
-- something is both an Alternative and a Monad.

-- Basic Laws.
-- like any monoid.
neutral_empty_law u = (empty <|> u) == u && (u <|> empty) == u

associative_law u v w = (u <|> (v <|> w)) == ((u <|> v) <|> w)

-- addtional Law to work with Monad.
-- This implies if a failure happens within a chain of monadic
-- computation the whole will fail.

monad_law1 f = (mzero >>= f) == mzero
monad_law2 m = (m >> mzero) == mzero

-- Useful functions.
-- alternative sum
my_asum :: (Alternative f, Foldable t) => t (f a) -> f a
my_asum = foldr (<|>) empty

-- guard
-- this can be used to mimic fitlering in list comprehension.

-- list comprehension version
my_guard :: Alternative m => Bool -> m ()
my_guard True = pure ()
my_guard _    = empty

pyth1 =
  [ (x, y, z)
    | z <- [1 ..],
      x <- [1 .. z],
      y <- [x .. z],
      x ^ 2 + y ^ 2 == z ^ 2
  ]

-- list monad -do block
-- very similar form with guard
-- Key point here:
--  1. guard will return empty if predicate is false.
--  2. by law
--       monad_law1 f = (mzero >>= f) == mzero
--       monad_law2 m = (m >> mzero) == mzero
--     any empty in the do block will cause the entire
--     computation return empty
--
--  3. guard return [()] when predicate is true.
--  4. look at the desugared version,
--     if my_guard(x^2 + y^2 == z^2) is empty, the computation
--     evaluated as emtpy immediately.
--     if my_guard(x^2 + y^2 == z^2) is [()], execution can
--     pass through, x, y, z are all in scope, thus return
--     works as usual.
--
--  Notice how wierd the list monad behaves. Nowhere specified
--  a loop like operation but it can traverse the entire list. its
--  because the mapping mechanism is implemented in list monad
--  and implied by >>= operator.

pyth2 = do
  z <- [1..]
  x <- [1..z]
  y <- [x..z]
  my_guard (x^2 + y^2 == z^2)
  return (x, y, z)

-- desugared version
pyth3 = do
  [1..] >>= \z ->
    [1..z] >>= \x ->
      [x..z] >>= \y ->
        my_guard (x^2 + y^2 == z^2) >> return (x, y, z)
