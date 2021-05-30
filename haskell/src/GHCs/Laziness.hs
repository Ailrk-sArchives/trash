module GHCs.Laziness where

-- haskell is not really lazy, it's non-strict.
--
-- Lazy: call by need, memoize everything.
-- Non-struct: call by name, don't need to memoize everything.
--
-- But this definition is a bit nit picky.

itsok = fst (1, undefined)
bomb = snd (1, undefined)   -- you can load this.

-- evalution order.
-- strict: inside out
-- non strict: outside in.

true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false= \a -> (\b -> b)

-- mightBoom true will force the evaluation inward of the definition.
-- >>> mightBoom true
-- 1
-- >>> mightBoom false
-- Prelude.undefined
mightBoom = \f -> f fst snd (1, undefined)

-- force strict evaluaton with seq

hypo :: IO ()
hypo = do
  let x :: Integer
      x = undefined     -- when it's lazily evalute this function is fine.
  s <- getLine
  case x `seq` s of     -- force to evaluate the bottom here.
    "hi" -> print x
    _    -> putStrLn "Hello"

-- this is ok. y is not forced, so seq is also not invoked.
wc x z = let y = undefined `seq` 'y' in x

-- create a chain that eventually trigger the undefined.
wc' x z = let y = undefined `seq` 'y' in let z = y `seq` 'z' in z

foo _ = 1

_ = foo undefined -- this is ok, _ pattern will not force evaluation.

{-@ use -ddump-simpl to see the "core dump" from GHC
   use -dsuppress-all to get cleaner output
   notice, seq will be compiled into a case expression. case
   are strict in core, so that make senses.
@-}

-- try it out
-- :set -dump-simpl

discriminatory1 :: Bool -> Int
discriminatory1 b =
  let x = undefined
   in case b of
        False -> 0
        True -> 1

discriminatory2 :: Bool -> Int
discriminatory2 b =
  let x = undefined
   in case x `seq` b of
        False -> 0
        True -> 1


-- 1. A thunk can have a bottom within.

-- call by name call by need
-- call by name is non-strict and call by need is full lazyness.
-- meaning you have full memoization for call by need.

{-@ try :sprint [1,2,3], this will print the fully evalutated list.
    this is because GHC will optimize the case when everything is just
    data constructor. data constructor is known as constant since they never
    change.

    Note: when data constructor is not applied it act as a function, but as it's
          applied it's treated as constant.

    So for :psrint [1,2,3], the whole thing is const, so evaluate to weak head normal
    form means evaluate the whole thing.
@-}

-- sharing
-- when the result of an evaluation is named, it can be safely shared by any other
-- uses without re-evaluation.

-- What promotes sharing?
--   same name

-- TODO
