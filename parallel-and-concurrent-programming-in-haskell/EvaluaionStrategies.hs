module EvaluaionStrategies where

import Control.Parallel.Strategies
import Control.DeepSeq

-- use strategies to modularize parallel code logic from
-- the parallelism code.
-- Strategy :: a -> Eval a

-- first create Strategy with the computation you want it to be
-- parallel
parPair'' :: Strategy (a, b)
parPair'' (a, b) = do
  a' <- rpar a
  b' <- rpar b
  return (a, b)

-- using this pattern we know
-- the result of (fib 2, fib 199) `using` parPair
-- is just the same as (fib 2, fib 199),
-- it's just the parallelized version
using' :: a -> Strategy a -> a
x `using'` s = runEval $ s x

-- parameterized strategy
evalPair' :: Strategy a -> Strategy b -> Strategy (a, b)
evalPair' sa sb (a, b) = do
  a' <- sa a
  b' <- sb b
  return (a', b')

-- define a new strategy
-- this strategy force the evaluation and wait for it to
-- finish.
rdeepseq' :: NFData a => Strategy a
rdeepseq' x = rseq (force x)

-- redefine pairPair' with evalPair. rpar is used as a strategy itself.
parPairWHNF :: Strategy (a, b)
parPairWHNF = evalPair' rpar rpar

-- now let's define a more generic parPair
parPair' :: Strategy a -> Strategy b -> Strategy (a, b)
parPair' sa sb = evalPair' (rparWith sa) (rparWith sb)

-- parPair with seq strategy.
parPairSeq :: (NFData a, NFData b) => Strategy (a, b)
parPairSeq = parPair' rdeepseq' rdeepseq'

-- Strategy for list --

evalList' :: Strategy a -> Strategy [a]
evalList' _ [] = return []
evalList' strat (x : xs) = do
  x' <- strat x
  xs' <- evalList' strat xs
  return $ x' : xs'


