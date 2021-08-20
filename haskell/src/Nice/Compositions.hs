{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}
module Nice.Compositions where

import           Control.Applicative        (many)
import           Control.Category           hiding ((.))
import           Control.Monad
import           Control.Monad.Trans.Except
import           Control.Monad.Trans.Reader
import           Control.Monad.Trans.State
import           Data.Char
import           Data.Function

-- how do you compose things other then simply use function composition?

-- There were a lot of fritctions to put haskell functions together until
-- you find out there're only limited amount of ways to compose things.

-- Guess PPL are too busy with fancy type systems and category theories,
-- there are very little talks focus on simple things like  techniques for
-- compistions.

-- However without having a good intuition of composition you can't write
-- large program at least easily or with any aestheic, because you're forced
-- to use do notation excessively, which make the code much more verbose.

-- Lambda calculus is terribly low level, but with various combinatoric logic
-- techniques we can combine nasty functions together to build bigger ones.
-- How to compose is partially about how to apply combinatoric logic in lambda
-- calculus.

-- Some important ideas that's helpful when doing compsitions:
-- 1. function as data (to feed into combinators).
--      Not just in the sense that you can pass a function or return a function,
--      but realize you have special combinators maek a function to another
--      function.
--      e.g .(.) is a binary operator that takes to function and give you
--      another function. For f :: a -> a it's a actually forms a commuative
--      monoid.
--
-- 2. Application is half function composition
--      Same as the difference with (<=<) and (=<<), (.) and ($) are really
--      the same thing.
--
-- 3. Everything are actually function application with some flavors.
--      <*> is ($) for applicative, (=<<) is application for monad.
--      e.g We define the special application operator (=<<) for monadic
--          function (a -> m b) which takes a value with type (m a).
--
--      and then youh ave (<<<), (>>>), whatever gives. They works on different
--      structure but follows one idea: Provide a combinator that takes two
--      function and return another.
--
-- 4. Composition in the large.
--      (.) doesn't only compose two functions, it's return value can be used
--      to further compose with other things. One compose with the other, and
--      eventually you get your entire function composed by small functions
--
--      One way to think about how functional program is to gluing small
--      functions with combinators:
--        1. Function is the basic element of composition
--        2. We write small functions implement features
--        3. We build bigger functions by composing them together.
--      We not only have control over functions that implements features, but
--      also gluing combiantors, because they're also functions.

-- PS:
-- On the other hand it's sometimes good to not to compose by hand but hand it
-- over to do notation. E.g in case you have a function taht takes 3 parameters
-- each need to be taken from a sequnce of process. In such case any
-- one liner composition will make code impossible to read, but with do notation
-- process of each paramter can be separate out, thus enhence the readability.

------------------------------------------------------------------------------
-- compose f :: a -> a
--   1. unary that maps to itself.
--   2. fold f get another function f' :: a -> a
add_dot :: String -> String
add_dot = (++ ".")

-- >>> f = foldr (.) add_dot (replicate 10 add_dot)
-- >>> f ""
-- "..........."

add_cross = (++"x")
add_o = (++"o")
add_newln = (++"\n")

-- use arrrow to better control the direction of composition.
bigendian1 = add_cross >>> add_cross >>> add_o
littlendian1 = add_cross <<< add_cross <<< add_o
-- >>> bigendian1 ""
-- >>> littlendian1 ""
-- "xxo"
-- "oxx"

-- >>> f = foldr (.) (\_ -> "") [add_o, add_cross, add_o]
-- >>> f ""
-- "oxo"

------------------------------------------------------------------------------
-- Think >>= as function application (for unary function)
--   normally f :: a -> b takes an a, f :: a -> m b still takes an a. but the
--   application is nolonger (*$)

square_root :: Double -> Maybe Double
square_root a
  | a >= 0 = Just $ sqrt a
  | otherwise = Nothing

infixr 8 $:
f $: a = a >>= f

-- >>> square_root $: (square_root 10)
-- Just 1.7782794100389228

-- >>> square_root $: square_root $: (return 10)
-- Just 1.7782794100389228

--
------------------------------------------------------------------------------ String -> Maybe String
add_dotm s = return (s++".")

add_crossm :: String -> Maybe String
add_crossm s = return (s++"x")

add_om :: String -> Maybe String
add_om s = return (s++"o")

g1 = foldr (>=>) add_dotm [add_crossm, add_om, add_crossm]
g1' = foldr (<=<) add_dotm [add_crossm, add_om, add_crossm]

-- >>> g1 ""
-- Just "xox."

-- >>> g1' ""
-- Just ".xox"

------------------------------------------------------------------------------
-- composition to transform from one type to another
-- f :: a -> b
data Nat = Z | S Nat deriving Show

natToInt :: Nat -> Int
natToInt Z     = 0
natToInt (S n) = 1 + natToInt n

intToNat :: Int -> Nat
intToNat 0 = Z
intToNat n = S (intToNat (n - 1))

int_literal :: Int -> String
int_literal n = show n ++ "d"

upper_case :: String -> String
upper_case = fmap toUpper

g2 :: Nat -> Nat
g2 = intToNat . length . add_dot . upper_case . int_literal . natToInt

-- >>> g2 (S . S . S . S $ Z)
-- S (S (S Z))

g2' :: Nat -> Nat
g2' = natToInt
  >>> int_literal
  >>> upper_case
  >>> add_dot
  >>> length
  >>> intToNat
-- >>> g2' (S . S . S . S $ Z)
-- S (S (S Z))

------------------------------------------------------------------------------
-- $ as >>=
--   f =<< a is application for monadic function, f <=< g is composition
--   for monadic function,
--   So for plain functions we have (.) for composition and ($) for application

-- this two are really the same thing.
foo1 = (\x -> return $ x + 1) =<< (\y -> return $ y + 10) =<< square_root 10
foo2 = (\x -> x + 1) $ (\y -> y + 10) $ sqrt 10

-- also in Data.Function we have backward application to miminc =<<

foo3 = square_root 10 >>= (\y -> return $ y + 10) >>= (\x -> return $ x + 1)
foo4 = sqrt 10
     & (\y -> y + 10)
     & (\x -> x + 1)

-- So application and composition are really the same thing in different context.
-- Pattern behind application:
--   take a funtion and it's parameter, give a value.
--   It compose because we have another function that takes this value.

------------------------------------------------------------------------------
-- application on applicative
-- gist of application is you can apply a function like (a -> b -> c)
-- through elements with effects.
-- Functor can only lift once, but app can keep going.

add3 :: Int -> Int -> Int -> Int
add3 a b c = a + b + c

-- >>> add3 1 2 3
-- 6

-- normal application get lifted
-- >>> pure add3 <*> Just 1 <*> Just 2 <*> Just 3
-- Just 6

-- ap supports partial application
-- >>> g1 = pure add3 <*> Just 1
-- >>> g1 <*> Just 2 <*> Just 3
-- Just 6

-- operations in applicative do compose.
acomp :: Applicative f => f (b -> c) -> f (a -> b) -> f (a -> c)
acomp a b = (fmap (.) a) <*> b

inc :: Int -> Int
inc = (+1)

dec :: Int -> Int
dec n = n - 1

decsome :: Applicative f => f (Int -> Int)
decsome = pure inc `acomp` pure dec `acomp` pure dec

compose_many_decsome :: Applicative f => f (Int -> Int)
compose_many_decsome = foldr1 acomp (replicate 10 decsome)

-- >>> compose_many_decsome <*> (Just 100)
-- Just 90

-- >>> nothing_change <*> Just 10
-- Just 10

------------------------------------------------------------------------------
-- You can also compose types.
-- You will hear "applicatives compose but monad don't", it's
-- trying to say composing one monad type with another not necessarily give
-- you yet another monad. It has nothing to do with term level applicaitons.

-- e.g The argument is about multiple layers of monads aren't necessarily
--     distribute
--     e.g  :: g (f a) -> f (g a) doesn't give you another monad
--     so for type like (f (g (f (g a))))
--     we cannot distribute and join to get (f (g a))

-- we can't joinn
type M a = (IO (Maybe (IO (Maybe a))))
-- this can join
type N a = (IO (IO (Maybe (Maybe a))))

-- this is a very different topic from examples listed above. Those are
-- patterns to work in term level.


------------------------------------------------------------------------------
-- monadic functions are still trivial.
-- In imperative programming you might have a counter inc 1 each time is called.
-- to do the same thing in haskell you all the sudden need a state monad.
-- but it's important to notice that except the normal function now becomes a
-- monadic function, nothing change.

type Counter a = State Int a

-- a function count number of applications
($#) :: (a -> b) -> a -> Counter b
f $# a = do
  modify (\i -> i + 1)
  return $ f a

-- just use it as normal $, but with Counter

foo5 :: Counter Int
foo5 = loop 10 (+1) 0
  where
    loop 0 _ _ = return 0
    loop n f a = do
      f $# a
      loop (n - 1) f a

-- One difference is you need to execute the side effect explicitly.
-- >>> execState foo5 0
-- 10

-- To be Continue..

