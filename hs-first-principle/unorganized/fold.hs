module Fold where

-- catamorphism

-- all this can be written by fold
-- they all have a base case as identity and then recursively build up.
sum0 :: [Integer] -> Integer
sum0 [] = 0
sum0 (x:xs) = x + sum0 xs

length0 :: [a] -> Integer
length0 [] = 0
length0 (_:xs) = 1 + length0 xs

product0 :: [Integer] -> Integer
product0 [] = 1
product0 (x:xs) = x * product0 xs

concat0 :: [[a]] -> [a]
concat0 [] = []
concat0 (x:xs) = x ++ concat0 xs

-- foldr, right associative
-- reduce to the right actually.
reducer :: (a -> b -> b) -> b -> [a] -> b
reducer f z [] = z
reducer f z (x:xs) = f x (reducer f z xs)

reducel :: (b -> a -> b) -> b -> [a] -> b
reducel f z [] = z
reducel f z (x:xs) = reducel f (f z x) xs

-- foldl has unconditional/forced recursion of the spine.
-- because the recursion happens on function passed in, not foldl it self.
-- foldl is bad for long or infinite list since it force evaluation.
-- use foldl' for foldl, which enforce the strict evaluation
-- strict evaluation means evaluate values inside con cells as it
-- traverses the spine, rather than accumulate unevaluated expressions.

scanl0 :: (a -> b -> a) -> a -> [b] -> [a]
scanl0 f q ls =
    q : (case ls of
           [] -> []
           x:xs -> scanl f (f q x) xs)


fibs = 1:scanl (+) 1 fibs
fibsN x = fibs !! x

-- scanl in reversed order.
facts = 1 : scanl (\x y -> case x == 2 of
                             True -> 6
                             False -> (*) ((+1) $ div x y) x)
                  2 facts







