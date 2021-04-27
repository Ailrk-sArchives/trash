{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-inline-rule-shadowing #-}
{-# OPTIONS_GHC -fenable-rewrite-rules #-}

module GHCs.Rules where

import Prelude hiding (foldl, foldr, map, sum)

{-@
    https://www.microsoft.com/en-us/research/wp-content/uploads/2001/09/rules.pdf
    https://wiki.haskell.org/GHC/Using_rules

    Rewrite rules, helps you to write specfic optimization.
    use --ddump-simpl-stats to see what rules fired.
    more details with --ddump-rule-firings.
    --ddump-rule-rewrites shows you what does the code look like
    before and after rewrite.
@-}
-- allow programmers to write domain specific optimizing rules.
-- e.g integerToInt (intToInteger x) = x
-- compiler doesn't knows the correlation between this two functions,
-- but we can add a rule to force this relationship.

-- although we will never hand write code like this one, but it's possible that
-- after some inlining some code ended up in this form.
-- If it's the case, rewrite rule can further shrink the code.

intToInteger :: Int -> Integer
intToInteger n = fromIntegral n

integerToInt :: Integer -> Int
integerToInt n = fromIntegral n

{-# RULES "intToInteger/id1" forall x. integerToInt (intToInteger x) = x #-}

{-# RULES "integerToInt/id2" forall x. intToInteger (integerToInt x) = x #-}

-- say we define a map now
map f [] = []
map f (x : xs) = f x : map f xs

-- and the compiler see this:
-- map f (map g xs)

-- we know it's just map (f . g) xs
-- This form is more efficent, the first solution has 2N runtime.
-- but compiler doesn't know.
--
-- What we want to do is essentially ask the compiler whenever find this pattern,
-- transform the code into what we want.

{-# RULES
"map/map" forall f g xs.
  map f (map g xs) =
    map (f . g) xs
  #-}

-- express identities programmers know true, but compiler doesn't.

-- THIS IS BAD. Compilation doesn't terminate.
foo x y = x

{-# RULES "commute" forall x y. foo x y = foo y x #-}

-- rewrite rules are embeded in hi files, the client code doesn't need to be aware of
-- it at all, but GHC still able to perform the rewrite.

-- short cut deforestation --
-- or list fusion.
-- We want to cut intermediate lists from problem.

foldr :: (a -> b -> b) -> b -> [a] -> b
foldr k z [] = z
foldr k z (x : xs) = k x (foldr k z xs)

build :: (forall b. (a -> b -> b) -> b -> b) -> [a]
build g = g (:) []

{-# RULES
"foldr/build" forall k z (g :: forall b. (a -> b -> b) -> b -> b).
  foldr k z (build g) =
    g k z
  #-}

-- use the rewrite rule to eliminate intermediate list.

sum :: [Int] -> Int
sum xs = foldr (+) 0 xs

down :: Int -> [Int]
down v = build (\c n -> down' v c n)

-- scott encoding of a list
down' 0 cons nil = nil
down' v cons nil = cons v (down' (v - 1) cons nil)

-- how will these get expanded?
-- sum (down 5)
--
-- ==> inline sum
-- foldr (+) 0 (down 5)
--
-- ==> inline down
-- foldr (+) 0 (build (down' 5))
--
-- ==> apply rewrite rule
-- (down' 5) (+) 0
val = sum (down 5)

-- another example:

threePartitions :: Int -> [(Int, Int, Int)]
threePartitions m =
  [ (i, j, k) | i <- [0 .. (m `div` 3)], j <- [i .. (m -1 `div` 2)], let k = m - (i * j) ]

-- fun fact, GHC will compile the list comprehension into a build call.
-- so anything with nested list comprehension can be benefit from using rewrite rule.


-- Quirks --
-- Things about compiler optimization is never that simple... Once you add one rule you need
-- to think about it's interaction with whole bunch of other optimization passes.
--
-- A fire of Rewrite rules might depends on some inline is triggerred.
-- but if a piece of code is inlined too much, it might miss the pattern that is rewriteble.
-- You need to know INLINE and rewrite quite well to be able to put them to the most use.

-- Phrases --
-- this is how you control the application of rules.
-- you can force some rules to be triggerd at phase 0, 1, or 2.
-- it seems so unflexible...
