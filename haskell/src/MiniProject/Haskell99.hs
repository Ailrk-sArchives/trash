module MiniProject.Haskell99 where

import           Data.Foldable
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
-- 8.   ----------------------------------------
-- 9.   ----------------------------------------
-- 10.  ----------------------------------------

{-@ Question 11 to 20 Lists, continued
@-}


{-@ Question 21 to 30 List again
@-}

{-@ Question 31 to 40 Arihtmeic
@-}

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
