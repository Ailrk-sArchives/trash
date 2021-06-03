module MiniProject.Haskell99 where

import           Data.Foldable
import           Data.List
import           Data.Traversable

import           Control.Monad.ST
import           Data.STRef
import           Debug.Trace

import           Control.Applicative
import           Control.Monad

import           Test.Hspec
import           Test.QuickCheck     ()

{-@ Question 1 to 10 List
@-}

-- 1.   ----------------------------------------
-- (*) Find the last element of a list.
-- (Note that the Lisp transcription of this problem is incorrect.)

-- >>> last_ [1..10]
-- 10
last_ :: [a] -> a
last_ []     = error "oh"
last_ [a]    = a
last_ (x:xs) = last_ xs

-- >>> last_' [1..10]
-- 10
last_' :: [a] -> a
last_' = foldr1 (flip const)

-- >>> last_'' ['a'..'z']
-- 'z'
last_'' :: [a] -> a
last_'' = head . reverse

-- >>> last_'' ['a'..'z']
-- 'z'
last_''' :: [a] -> a
last_''' = foldr1 (curry snd)

-- 2.   ----------------------------------------
-- (*) Find the last but one element of a list.

-- >>> butLast_ [1..10]
-- 9
butLast_ :: [a] -> a
butLast_ []       = error "oh"
butLast_ [a]      = error "oh"
butLast_ (a:x:[]) = a
butLast_ (x:xs)   = butLast_ xs


-- >>> butLast_' [1..10]
-- 9
butLast_' :: [a] -> a
butLast_' = last . init


-- >>> butLast_'' [1..10]
-- 9
butLast_'' :: [a] -> a
butLast_'' = head . tail . reverse


-- 3.   ----------------------------------------
-- (*) Find the K'th element of a list. The first element in the list is number 1.

-- >>> elementAt_ [1..10] 2
-- 3
-- >>> elementAt_ [1..10] (-1)
-- index can't be smaller than 0
elementAt_ :: [a] -> Int -> a
elementAt_ [] _ = error "empty list"
elementAt_ (x:xs) n
  | n == 0 = x
  | n < 0 = error "index can't be smaller than 0"
  | otherwise = elementAt_ xs (n - 1)

-- >>> elementAt_' [1..10] 2
-- 3
elementAt_' :: [a] -> Int -> a
elementAt_' xs n = xs !! n


-- >>> elementAt_'' [1..10] 2
-- 2
elementAt_'' :: [a] -> Int -> a
elementAt_'' xs n
  | n < 0 = error "n should be larger than 0"
  | otherwise = snd . head . dropWhile (\(idx, _) -> idx < n) $ (zip [1..] xs)



-- 4.   ----------------------------------------
-- (*) Find the number of elements of a list.

-- >>> length_ [1..10]
-- 10
length_ :: [a] -> Int
length_ []     = 0
length_ (x:xs) = 1 + (length_ xs)

-- >>> length_' [1..10]
-- 10
length_' :: [a] -> Int
length_' = foldr (\_ b -> b + 1) 0

-- >>> length_'' [1..10]
-- 10
length_'' :: [a] -> Int
length_'' = sum . (fmap $ const 1)


-- 5.   ----------------------------------------
-- (*) Reverse a list.

-- >>> reverse_ [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
reverse_ :: [a] -> [a]
reverse_ []     = []
reverse_ (x:xs) = (reverse_ xs) ++ [x]

-- >>> reverse_' [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
reverse_' :: [a] -> [a]
reverse_' = foldr (\a b -> b ++ [a]) []

-- >>> reverse_'' [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
reverse_'' :: [a] -> [a]
reverse_'' = foldl (\b a -> a:b) []

-- >>> reverse_''' [1..10]
-- [10,9,8,7,6,5,4,3,2,1]
reverse_''' :: [a] -> [a]
reverse_''' xs = runST $ do
  listRef <- sequence $ fmap newSTRef xs
  let len = length_ xs
      mid = (len `div` 2) - 1
  traverse_ (\i -> swap (listRef !! i) (listRef !! (len-1-) i)) [0..mid]
  traverse readSTRef listRef
  where
    swap :: STRef s a -> STRef s a -> ST s ()
    swap ref1 ref2 = do
      r1 <- readSTRef ref1
      r2 <- readSTRef ref2
      writeSTRef ref2 r1
      writeSTRef ref1 r2
      return ()


-- 6.   ----------------------------------------
-- (*) Find out whether a list is a palindrome. A palindrome can be read forward or backward;
--     e.g. (x a m a x).

-- >>> isPalindrome [1, 2, 3, 2, 1]
-- True
-- >>> isPalindrome [1, 2, 3, 2, 1, 2]
-- False
isPalindrome :: Eq a => [a] -> Bool
isPalindrome xs = xs == (reverse xs)

-- >>> isPalindrome' [1, 2, 3, 2, 1]
-- True
isPalindrome' :: Eq a => [a] -> Bool
isPalindrome' = liftA2 (==) id reverse


-- 7.   ----------------------------------------
--  (**) Flatten a nested list structure.
-- Transform a list, possibly holding lists as elements into
-- a `flat' list by replacing each list with its elements (recursively).

data NestedList a = E a | L [NestedList a]

-- >>> flattern (E 5)
-- [5]
--
-- >>> flattern (L [E 1, L [E 2, L [E 3, E 4], E 5]])
-- [1,2,3,4,5]
--
-- >>> flattern (L [])
-- []
flattern :: NestedList a -> [a]
flattern (L [])        = []
flattern (E x)         = [x]
flattern (L (E x: xs)) = x : (mconcat . fmap flattern $ xs)


-- concatMap map f to each elements and concat the result.
flattern' :: NestedList a -> [a]
flattern' (E x)  = [x]
flattern' (L xs) = concatMap flattern' xs

-- foldMap can be thought as the generalization of concatMap.
flattern'' :: NestedList a -> [a]
flattern'' (E x)  = [x]
flattern'' (L xs) = foldMap flattern'' xs

-- bind for list is just concatMap.
flattern''' :: NestedList a -> [a]
flattern''' (E x)  = return x
flattern''' (L xs) = xs >>= flattern'''



-- 8.   ----------------------------------------
-- (**) Eliminate consecutive duplicates of list elements.

-- >>> compress "aaaabccaadeeee"
-- "abcade"
--
compress :: Eq a => [a] -> [a]
compress = foldr op []
  where
    op a b
      | b == [] = a : b
      | otherwise = case b of
                      x:xs -> if x == a then x:xs else a:x:xs


-- >>> compress' "aaaabccaadeeee"
-- "abcade"

compress' :: Eq a => [a] -> [a]
compress' []          = []
compress' t@(x:xs) = let rest = (dropWhile (==x) t)
                      in x:compress' rest

-- >>> compress' "aaaabccaadeeee"
-- "abcade"
compress'' :: Eq a => [a] -> [a]
compress'' = map head . group



-- 9.   ----------------------------------------
-- (**) Pack consecutive duplicates of list elements into sublists.
-- If a list contains repeated elements they should be placed in separate sublists.

-- >>> pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]

pack :: Eq a => [a] -> [[a]]
pack = foldr op []
  where
    op a [] = [[a]]
    op a (b:bs)
      | a == head b = (a : b) : bs
      | otherwise = [a] : (b:bs)

-- span separate a list into two, the first half stop at the first element satisfies
-- predicate.
--
-- A typical use case is to recursively separate a list until empty.
--
-- >>> pack' ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
-- ["aaaa","b","cc","aa","d","eeee"]
pack' :: Eq a => [a] -> [[a]]
pack' (x:xs) = let (first, rest) = span (==x) xs
                in (x:first) : pack rest
pack' [] = []


-- 10.  ----------------------------------------
-- (*) Run-length encoding of a list. Use the result of problem P09 to implement the
-- so-called run-length encoding data compression method. Consecutive duplicates of
-- elements are encoded as lists (N E) where N is the number of duplicates of the element E.

-- >>> encode "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--
encode :: Eq a => [a] -> [(Int, a)]
encode = foldr op []
  where
    op a [] = [(1, a)]
    op a xs@((i, b):bs)
      | a == b = (i + 1, b) : bs
      | otherwise = (1, a) : xs


-- >>> encode' "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
encode' :: Eq a => [a] -> [(Int, a)]
encode' = fmap (\a -> (length a, head a)) . group

-- use applicative to partially apply
-- >>> encode'' "aaaabccaadeeee"
-- [(4,'a'),(1,'b'),(2,'c'),(2,'a'),(1,'d'),(4,'e')]
--
encode'' :: Eq a => [a] -> [(Int, a)]
encode'' = fmap ((,) <$> length <*> head) . group


{-@ Question 11 to 20 Lists, continued
@-}

-- 11.  ----------------------------------------
-- (*) Modified run-length encoding.

-- 12.  ----------------------------------------
-- (**) Decode a run-length encoded list.


-- 13.  ----------------------------------------
-- (**) Run-length encoding of a list (direct solution).


-- 14.  ----------------------------------------
-- (*) Duplicate the elements of a list.


-- 15.  ----------------------------------------
-- (**) Replicate the elements of a list a given number of times.


-- 16.  ----------------------------------------
-- (**) Drop every N'th element from a list.



-- 17.  ----------------------------------------
-- (*) Split a list into two parts; the length of the first part is given.



-- 18.  ----------------------------------------
-- (**) Extract a slice from a list.


-- 19.  ----------------------------------------
-- (**) Rotate a list N places to the left.



-- 20.  ----------------------------------------
-- (*) Remove the K'th element from a list.



{-@ Question 21 to 28 List again
@-}

-- 21.  ----------------------------------------

-- 22.  ----------------------------------------

-- 23.  ----------------------------------------

-- 24.  ----------------------------------------

-- 25.  ----------------------------------------

-- 26.  ----------------------------------------

-- 27.  ----------------------------------------

-- 28.  ----------------------------------------

{-@ Question 31 to 40 Arihtmeic
@-}

-- 29.  ----------------------------------------

-- 30.  ----------------------------------------

-- 31.  ----------------------------------------

-- 32.  ----------------------------------------

-- 33.  ----------------------------------------

-- 34.  ----------------------------------------

-- 35.  ----------------------------------------

-- 36.  ----------------------------------------

-- 37.  ----------------------------------------

-- 38.  ----------------------------------------

-- 39.  ----------------------------------------

-- 40.  ----------------------------------------

-- 41.  ----------------------------------------


{-@ Question 41 to 50 logic and codes
@-}

{-@ Question 41 to 50 binary trees
@-}

{-@ Question 41 to 50 binary trees continued
@-}

{-@ Question 41 to 50 multiway trees
@-}

{-@ Question 41 to 50 graphs
@-}

{-@ Question 41 to 50 Miscellaneous problems
@-}

{-@ Question 41 to 50 Miscellaneous continued
@-}
