{-# LANGUAGE StandaloneDeriving #-}
module Libs.TraversableFoldable where

import           Data.Foldable
import           Data.Monoid

import           Data.Ix

-- the namespace of traversable and foldable is
-- really messy in haskell, as important combinators
-- scatter around different modules.

xs = [1, 2, 3, 4, 5, 10]


-- fmap first
t1 = fmap (+1) xs

-- traverse
t2 = traverse putStrLn (fmap show xs)

-- fold
t4 = foldl' (-) 0 xs -- -21
-- foldl is strict by nature. you can perform op and get a value
-- immediately.
-- foldl means we folding to the left. In this  example we folding
-- towards the direction of the base case.
-- (((0 - 1) - 2) - 3 ...)

t3 = foldr (-) 0 xs  -- -3
-- foldr goes finite, it's lazy by nature.
-- foldr means folding to the right. In this case we fold to the
-- uneavluated expression.
-- Thunks will accumulate and eventually bloat the stack.
-- (1 - (2 - (3 - ... (6 - 0))))

{-@ Which fold to use?
    foldr: right foldr works on infinite lists as it's shown in the example above.
           foldr matches on the structure of the list, so it's can be efficient when
           working with list.

           In constrast left fold will never return, because it will try to apply op
           to all elements first then do the evalution.

    foldl': Strict version of left fold.

    foldl: just don't use it. It doesn't make sense to have a lazy left fold.
           what it does is it build up bunch of thunks when applying foward, and
           when it evaluates back those thunk, values will accumulate in the stack
           and you get stack overfloat.
@-}


-- scanl prefix sum
t5 = scanl (-) 0 xs
t6 = scanr (-) 0 xs

-- from Data.Foldable

{-@ fold and foldMap
    if a list is not a list of monoid already, you have two choices:
       1. fmap a function (f :: a -> m) on the list then fold it
       2. foldMap the list with (f :: a -> m)
    foldMap is just a shorthand to convert all elements into monoids and
    then fold.

    For folding elements without a base, being a monoid is important.

    In imperative code it's also sometime awkward that you can't have a empty
    value as the accumulator.
@-}

-- fold a list of monoid.
-- it's the same as mconcat.
t7 = getProduct $ fold (fmap Product xs)
t8 = getProduct $ mconcat (fmap Product xs)

-- foldMap just convert t a to t m and fold on it.
-- with foldMap it's even shorter.
t9 = getProduct $ foldMap Product xs

-- Foldable.toList convert a Foldable to a list
-- every Foldable is isomorphic to some list.
t10 = toList (Just 1)

-- Some short hands for foldables.
t11 = maximum xs
t12 = min xs
t13 = sum xs
t14 = product xs

-- foldl1 and foldr1 versions that doesn't need base but the length of the
-- list must greater than 1.
t15 = foldl1 (-) xs
t16 = foldr1 (-) xs


{-@ traverse is mapM
    traverse_ discard the content.

    for_ is flip traverse_.
@-}
-- traverse_ :: (a -> f b) -> t a -> f (t b)
-- map into a foldable and combine effects.
f17 = traverse_ putStrLn (fmap show xs)

-- worth mentioning mapM_ is the same as traverse_
-- bascially traverse is mapM, it's a map that performs some computation on
-- each element, and combine all effects together at the end.
f17' = mapM_ putStrLn (fmap show xs)

-- use for_ or traversable depends on which expresion is longer.
-- the function or the foldable.
f18 = for_ (fmap show xs) putStrLn
f18' = forM_ (fmap show xs) putStrLn

-- foldr that perform some actions on it's binop.
f19 = foldrM (\a b -> if a > b then Just (a + b) else Nothing ) 0 xs
f19' = foldrM (\a b -> if a /= b then Just (a + b) else Nothing ) 0 xs

-- common utility from foldable.
f21 = find (==1) xs
f22 = notElem 1 xs

-- and or / any all, very common
f23 = and (fmap (\x -> x `mod` 2 == 0) xs)
f24 = or (fmap (\x -> x `mod` 2 == 0) xs)
f25 = any (\a -> a `mod` 2 == 0) xs
f26 = all (\a -> a `mod` 2 == 0) xs

-- what is sequence?
-- Evaluating each element of the traversable and collecting the result.
-- Why it's it called sequencing?
-- It's the same as semi colon in imperative langauge. Ref to materials on the
-- operational semantics of SIMP (simple imperative programming langauge), the
-- operator ; is used as separator in a list of statement.
--
-- So in our case, sequence can be though as perform all sequencing operator ;
-- at once, or really just run the program.
f20 = sequence (fmap (putStrLn . show) xs)
f20' = sequenceA (fmap (putStrLn . show) xs)
f20''' = sequence_ (fmap (putStrLn . show) xs)
f20'''' = sequenceA_ (fmap (putStrLn . show) xs)


{-@ Some other functions in -base-.
@-}

-- a generator.
f27 = take 10 $ iterate (+1) 1

-- infinite generator
-- drop and take are dual.
f28 = drop 5 $ take 10 $ repeat 1

f29 = replicate 10 10

-- make infinite circular list.
-- useful for things like mod index.
f30 = cycle [1, 2, 3]

-- span separat the list from the frist element that the predicate
-- is false.
f31 = span (< 3) xs
f32 = splitAt 3 xs
f33 = zip xs xs
f34 = zip3 xs xs xs
f35 = zipWith (+) xs xs
f36 = unzip (zip xs xs)


{-@ Ix@-}

-- ok we do have range.
f37 = range (1, 10)
f38 = index (1, 10) 3
f39 = inRange (1, 10) (zipWith3 (\a b c -> a + b + c) xs xs xs !! 0)
f40 = rangeSize (0, length xs)

-- We can derive Ix from a Enum, which is pretty nice.
data Color = Red | Orange | Yellow | Green | Blue | Indigo | Violet deriving (Enum, Ord, Eq, Show)
deriving instance Ix Color

f41 = range (Red, Yellow)
