{-# OPTIONS_GHC -Wno-deferred-type-errors #-}

-- https://www.cs.purdue.edu/homes/rompf/papers/cong-icfp19.pdf
-- https://en.wikipedia.org/wiki/Defunctionalization
-- https://www.youtube.com/watch?v=vNwukfhsOME
-- https://ncatlab.org/nlab/show/defunctionalization
-- https://homepages.inf.ed.ac.uk/wadler/papers/papers-we-love/reynolds-definitional-interpreters-1998.pdf
-- http://www.mlton.org/References.attachments/060916-mlton.pdf

-- Def: convert higher order function in to data type.
-- so you don't have higher order fuction in the program
-- Then you implement an apply function to interpret the data type.
-- this way you don't need to pass function but just passing data.

-- In addition, if you do a cps transformation, the rest of the program will be a higher order
-- function.
-- So you can defunctionalize that, to represent the rest of computation as data type.

-- Application in compiler:
--  CFG and SSA are hard to build for higher order program. But we can transform all
--  higher order programs into first order one  by defuntionalization.
--  lambda terms.
--
--  example:
--  (let ((f (foo 7 g k)
--        (g (aref a7 i ))))
--     (if (< i j (h 30) (f h))))
--  To build a CFG for flow analysis, you need to analyse all possible lambda for f and h.
--  but itself is a flow analysis problem.

--  Goal:
--    1. eliminate higher order function.
--    2. make direct toplevel call, which are easy to optemize.
--    3. make control flow into available to rest of optimizer.
--    4. optimize closure justlike other data structures.
--
-- App:
--    1. flow analysis optimizaiton
--    2. common subexpression elimination
--    3. redundant assignment detection
--    4. code hoisting
--    5 .type inference
--    6. verification


module Other.Defunctionalization where


-- 1. convert predicate functon into datatypes.
-- notice defunctionalization is closed under the predicate, we can only use
-- datatypes we defined.
-- This is good for serialization.

data Filter
  = Tat
  | IsOdd
  | IsEven
  | LessThan Int
  | And Filter Filter

apply :: Filter -> (Int -> Bool)
apply Tat          = const True
apply IsOdd        = odd
apply IsEven       = even
apply (LessThan n) = (< n)
apply (And l r)    = \x -> apply l x && apply r x

filterDefun :: Filter -> [Int] -> [Int]
filterDefun _ [] = []
filterDefun f (x:xs) =
  if apply f x
     then x : filterDefun f xs
     else filterDefun f xs

-- 2. tree

data Tree a = Leaf a | Node (Tree a) (Tree a)

-- original program
-- walk though the tree and collect elements into a list
cons :: a -> [a] -> [a]
cons x xs = x : xs

o :: (b -> c) -> (a -> b) -> a -> c
o f g x = f (g x)

flattern :: Tree t -> [t]
flattern t = walk t []

walk :: Tree t -> [t] -> [t]
walk (Leaf x)     = cons x
walk (Node t1 t2) = walk t1 `o` walk t2

-- defuntionalized:
-- represent cons and o as data, then interpret it with applyCons.

data Lam a = LamCons a
           | LamO (Lam a) (Lam a)

applyCons :: Lam a -> [a] -> [a]
applyCons (LamCons x) xs  = x : xs
applyCons (LamO t1 t2) xs = applyCons t1 (applyCons t2 xs)

consDef :: a -> Lam a
consDef = LamCons

oDef :: Lam a -> Lam a -> Lam a
oDef = LamO

flatternDef :: Tree t -> [t]
flatternDef t = applyCons (walkDef t) []

walkDef :: Tree t -> Lam t
walkDef (Leaf x)     = consDef x
walkDef (Node t1 t2) = oDef (walkDef t1) (walkDef t2)

-- ps: cps

data Wood a = Foliage | Branch (Wood a) (Wood a)

depth1 :: Wood a -> Int
depth1 = loop
  where
    loop tree =
      case tree of
        Foliage    -> 1
        Branch l r -> succ (max (loop l) (loop r))

-- cps transformed
-- we know loop on right tree finish before loop on first tree for sure.
depth2 :: Wood a -> Int
depth2 = flip loop id where
  loop cons k = case cons of
                  Foliage -> k 1
                  Branch l r ->
                    loop l $ \kl ->
                    loop r  $ \kr -> succ (max kl kr)

-- one way to thin about cps.
-- loop l k
-- loop l will return something. This something need to be obtained somehow.
-- by passing a k that takes the argument we are able to use the result of
-- loop l.

-- 3. defunctionaliza the continuation
-- related topic: Closure conversion

-- Represent continuations as data types.

data DCont a
  = DContL (Tree a) (DCont a)   -- first eval left child.
  | DContR Int (DCont a)        -- when DContR works left child should be evaled
  | DContID                     -- end case.

depth3 :: Wood a -> Int
depth3 tree = loop tree DContID where
  loop cons k = case cons of
                  Foliage    -> eval k 1
                  Branch l r -> loop l (DContL r k)

  eval cons d = case cons of
                  DContL r k  -> loop r (DContR d k)
                  DContR dl k -> eval k (succ (max dl d))
                  DContID     -> d
