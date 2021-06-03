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
-- Insert an element at a given position into a list.



-- 22.  ----------------------------------------
-- Create a list containing all integers within a given range.



-- 23.  ----------------------------------------
-- Extract a given number of randomly selected elements from a list.



-- 24.  ----------------------------------------
-- Lotto: Draw N different random numbers from the set 1..M.



-- 25.  ----------------------------------------
-- Generate a random permutation of the elements of a list.



-- 26.  ----------------------------------------
-- (**) Generate the combinations of K distinct objects chosen from the N elements of a list



-- 27.  ----------------------------------------
-- Group the elements of a set into disjoint subsets.



-- 28.  ----------------------------------------
-- Sorting a list of lists according to length of sublists



{-@ Question 31 to 40 Arihtmeic
@-}
-- 31.  ----------------------------------------
-- (**) Determine whether a given integer number is prime.



-- 32.  ----------------------------------------
-- (**) Determine the greatest common divisor of two positive integer numbers. Use Euclid's algorithm.


-- 33.  ----------------------------------------
-- (*) Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.



-- 34.  ----------------------------------------
-- (**) Calculate Euler's totient function phi(m).



-- 35.  ----------------------------------------
-- (**) Determine the prime factors of a given positive integer. Construct a flat list containing the prime factors in ascending order.



-- 36.  ----------------------------------------
-- (**) Determine the prime factors of a given positive integer.



-- 37.  ----------------------------------------
-- (**) Calculate Euler's totient function phi(m) (improved).



-- 38.  ----------------------------------------
-- (*) Compare the two methods of calculating Euler's totient function.



-- 39.  ----------------------------------------
-- (*) A list of prime numbers.



-- 40.  ----------------------------------------
-- (**) Goldbach's conjecture.



-- 41.  ----------------------------------------
-- (**) Given a range of integers by its lower and upper limit, print a list of all even numbers and their Goldbach composition.



{-@ Question 46 to 50 logic and codes
@-}

-- 46.  ----------------------------------------
-- (**) Define predicates and/2, or/2, nand/2, nor/2, xor/2, impl/2 and equ/2
-- (for logical equivalence) which succeed or fail according to the result of their
-- respective operations; e.g. and(A,B) will succeed, if and only if both A and B succeed.

-- 47.  ----------------------------------------
-- (*) Truth tables for logical expressions (2).

-- 48.  ----------------------------------------
-- (**) Truth tables for logical expressions (3).

-- 49.  ----------------------------------------
-- (**) Gray codes.


-- 49.  ----------------------------------------
-- (***) Huffman codes.


{-@ Question 54A to 60 binary trees
@-}

data Tree a = Empty | Branch a (Tree a) (Tree a) deriving (Show, Eq)

branch x l r = Branch x l r
leaf x = Branch x Empty Empty

-- 54A.  ---------------------------------------
-- Check whether a given term represents a binary tree

-- 55.  ----------------------------------------
-- Construct completely balanced binary trees

-- 56.  ----------------------------------------
-- Symmetric binary trees

-- 57.  ----------------------------------------
-- Binary search trees (dictionaries)

-- 58.  ----------------------------------------
-- Generate-and-test paradigm

-- 59.  ----------------------------------------
-- Construct height-balanced binary trees


-- 60.  ----------------------------------------
-- Construct height-balanced binary trees with a given number of nodes


{-@ Question 61 to 69 binary trees continued
@-}

-- 61.  ----------------------------------------
-- Count the leaves of a binary tree


-- 61A.  ---------------------------------------
-- Collect the leaves of a binary tree in a list


-- 62.  ----------------------------------------
-- Collect the internal nodes of a binary tree in a list



-- 62B. ----------------------------------------
-- Collect the nodes at a given level in a list



-- 63.  ----------------------------------------
-- Construct a complete binary tree



-- 64.  ----------------------------------------
--  a layout algorithm for binary tree 1


-- 65.  ----------------------------------------
--  another layout algorithm for binary tree 1

-- 66.  ----------------------------------------
--  yet another layout algorithm for binary tree 1

-- 67A. ----------------------------------------
-- A string representation of binary trees


-- 68.  ----------------------------------------
-- Preorder and inorder sequences of binary trees.
-- We consider binary trees with nodes that are identified by single lower-case letters,
-- as in the example of problem P67.


-- 69.  ----------------------------------------
-- Dotstring representation of binary trees.



{-@ Question 70B to 73 multiway trees
@-}
data MTree a = Node a [MTree a] deriving (Eq, Show)


-- 70B. ----------------------------------------
-- (*) Check whether a given term represents a multiway tree.

-- 70C. ----------------------------------------
-- (**) Tree construction from a node string.

-- 71.  ----------------------------------------
-- (*) Determine the internal path length of a tree.

-- 72.  ----------------------------------------
-- (*) Construct the bottom-up order sequence of the tree nodes.

-- 73.  ----------------------------------------
-- (**) Lisp-like tree representation.


{-@ Question 80 to 89 graphs
@-}

-- 80.   ----------------------------------------
-- (***) Conversions

-- 81.   ----------------------------------------
-- (**) Path from one node to another one


-- 82.   ----------------------------------------
-- (*) Cycle from a given node


-- 83.   ----------------------------------------
-- (**) Construct all spanning trees


-- 84.   ----------------------------------------
-- (**) Construct the minimal spanning tree



-- 85.   ----------------------------------------
-- (**) Graph isomorphism


-- 86.   ----------------------------------------
-- (**) Node degree and graph coloration


-- 87.   ----------------------------------------
-- (**) Depth-first order graph traversal (alternative solution)


-- 88.   ----------------------------------------
-- (**) Connected components (alternative solution)


-- 89.   ----------------------------------------
-- (**) Bipartite graphs



{-@ Question 90 to 94 Miscellaneous problems
@-}
-- 90.   ----------------------------------------
-- (**) Eight queens problem


-- 91.   ----------------------------------------
-- (**) Knight's tour


-- 92.   ----------------------------------------
-- (***) Von Koch's conjecture

-- 93.   ----------------------------------------
-- (***) An arithmetic puzzle


-- 94.   ----------------------------------------
-- (***) Generate K-regular simple graphs with N nodes


{-@ Question 85 to 99 Miscellaneous continued
@-}


-- 95.   ----------------------------------------
-- (**) English number words


-- 96.   ----------------------------------------
-- (**) Syntax checker


-- 97.   ----------------------------------------
-- (**) Sudoku

-- 98.   ----------------------------------------
-- (***) Nonograms


-- 99.   ----------------------------------------
-- (***) Crossword puzzle

