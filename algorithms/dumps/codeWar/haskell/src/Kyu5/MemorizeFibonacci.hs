-- <Memoized Fibonacci>

module Kyu5.MemorizeFibonacci where
import Test.Hspec
import Test.QuickCheck

-- Problem Context

-- The Fibonacci sequence is traditionally used to explain tree recursion.

-- fibonacci 0 = 0
-- fibonacci 1 = 1
-- fibonacci n = fibonacci (n-1) + fibonacci (n-2)

-- This algorithm serves welll its educative purpose but it's tremendously
--  inefficient, not only because of recursion, but because we invoke
-- the fibonacci function twice, and the right branch of recursion (i
-- .e. fibonacci(n-2)) recalculates all the Fibonacci numbers already
-- calculated by the left branch (i.e. fibonacci(n-1)).

-- This algorithm is so inefficient that the time to calculate any Fibonacci
-- number over 50 is simply too much. You may go for a cup of coffee
-- or go take a nap while you wait for the answer. But if you try it
-- here in Code Wars you will most likely get a code timeout before any answers.

-- For this particular Kata we want to implement the memoization solution
-- . This will be cool because it will let us keep using the tree recursion
-- algorithm while still keeping it sufficiently optimized to get an answer very rapidly.

-- The trick of the memoized version is that we will keep a cache data
-- structure (most likely an associative array) where we will store the
-- Fibonacci numbers as we calculate them. When a Fibonacci number is
-- calculated, we first look it up in the cache, if it's not there, we
-- calculate it and put it in the cache, otherwise we returned the cached number.

-- Refactor the function into a recursive Fibonacci function that using
-- a memoized data structure avoids the deficiencies of tree recursion
-- Can you make it so the memoization cache is private to this function?

-- 2019-11-21
-------------------------------------------------------------
-- first attempt
-------------------------------------------------------------
import Data.Map as M
import Data.Maybe
import Control.Monad.Trans.State
import Control.Applicative (liftA2)

type FibCacheState = State (M.Map Int Integer, Int) (Maybe Integer)

-- each time you run the state it take s and compute new value
initCache :: M.Map Int Integer
initCache = M.fromList [(0, 0), (1, 1)]

fib :: FibCacheState
fib = state $
  \(cache, n) ->
    let n' = length cache - 1
     in if n <= n'  -- fib(n) cached, retrived it.
           then (M.lookup n cache, (cache, n))
        else let (left, (leftcache, _)) = runState fib (cache, n-2)
                 (right, (rightcache, _)) = runState fib (leftcache, n-1)
                 res = liftA2 (+) left right
                 newcache =
                   case M.lookup n rightcache of
                     Nothing -> M.insert n (fromMaybe 0 res) rightcache
                     _ -> rightcache

                in (res, (newcache, n))

fibonacci :: Int -> Integer
fibonacci n = fromMaybe 0 $ evalState fib (initCache, n)



-------------------------------------------------------------
-- best practice
-------------------------------------------------------------

fibonacci' :: Int -> Integer
fibonacci' n = fibs !! n
  where fibs = 0 : zipWith (+) fibs (tail fibs)

-- testing --------------------------------------------------------------------

spec :: Spec
spec = do
  it "Fixed tests" $ do
    fibonacci 70 `shouldBe` 190392490709135
    fibonacci 60 `shouldBe` 1548008755920
    fibonacci 50 `shouldBe` 12586269025
