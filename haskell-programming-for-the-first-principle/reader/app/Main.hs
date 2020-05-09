module Main where

import Control.Applicative
import Data.Char as C

----------------------------------------------------------
boop = (*2)
doop = (+10)

bip :: Integer -> Integer
bip = boop . doop

-- FUNCTORIAL CONTEXT
-- map the function (a -> b) over antoher function
-- (-> a) b, treat the second function as an applicative
bloop :: Integer -> Integer
bloop = fmap boop doop
-- fmap boop doop x = boop (doop x)

-- APPLICATIVE CONTEXT
bbop :: Integer -> Integer
bbop = (+) <$> boop <*> doop

duwop :: Integer -> Integer
duwop = liftA2 (+) boop doop


-- ((+) <$> boop <*> doop) x = (boop x) + (doop x)

-- An monadic context which doing the same thing
-- combine the (-> a) structure into one.
boopDoop :: Integer -> Integer
boopDoop = do
    a <- boop
    b <- doop
    return (a + b)

-- Reader: astract out function application and gives us a wau to
-- do compuation in terms of an argument that hasn't been supplied yet.
-- It is a way to avoid explicit argument passing.
----------------------------------------------------------
----------------------------------------------------------
-- Warmup EX
cap :: String -> String
cap = map toUpper

rev :: String -> String
rev = reverse

composed :: String -> String
composed = cap . rev

fmapped :: String -> String
fmapped = fmap cap rev

-- with applicative context
tupled :: String -> (String, String)
tupled = liftA2 (,) cap rev

tupled' :: String -> (String, String)
tupled' = (,) <$> cap <*> rev

-- with monadic context
tupled'' :: String -> (String, String)
tupled'' = do
    a <- cap
    b <- rev
    return (a, b)

tupled''' :: String -> (String, String)
tupled''' xs = rev <$> (cap >>= (,)) xs
-- cap >>= (,) xs) = (cap xs, xs)
-- rev <$> (cap >>= (,)) xs = rev <$> (\xs -> (cap xs, xs))
--
-- This function has two major steps, bind and fmap.
-- (,) :: a -> b -> (a, b)
-- bind: take a monadic structure and map it into another monad
--   (>>=) :: Monad m => m a -> (a -> m b) -> m b
--     Here (m a) is a function, so its signature is
--            m     a
--          (-> a0) a1
--
--     (a -> m b) is (,), can be written as
--            m         b
--          (-> a1) (a2 -> (a1, a2)),
--     when a in m a be shoved into (a -> mb), it becomes
--          (a2 -> (a1, a2))
--     it is a function return a function return a tuple.
--     This can be confirmed by signiture:
--          (cap >>= (,)) :: String -> (String, String)
--
-- applying (cap >>= (,)) xs
--   xs is applied into (a2 -> (a1, a2)), now the how term is a tuple
--
-- fmap:
--   (<$>) :: Functor f => (a -> b) -> f a -> f b
--   We now need to fmap the function (a -> b) on ((,) a) b
--
--
--

tupled'''' :: String -> (String, String)
tupled'''' xs = cap <$> (rev >>= (,)) xs

tupled''''' :: String -> (String, String)
tupled''''' = cap <$> rev >>= (,)

main :: IO ()
main = undefined
