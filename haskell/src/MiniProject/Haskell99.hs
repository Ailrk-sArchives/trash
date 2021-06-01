module MiniProject.Haskell99 where

import           Data.Foldable
import           Data.Traversable

import           Test.Hspec
import           Test.QuickCheck

{-@ Question 1 to 10 List
@-}

-- 1.
-- (*) Find the last element of a list.
-- (Note that the Lisp transcription of this problem is incorrect.)

q1 :: [a] -> a
q1 []     = error "oh"
q1 [a]    = a
q1 (x:xs) = q1 xs

q1' :: [a] -> a
q1' = foldr1 (flip const)

q1'' :: [a] -> a
q1'' = head . reverse

t1 = q1 [1..10]
t2 = q1 ['a'..'z']

-- 2.
--

q2 :: [a] -> a
q2 []       = error "oh"
q2 [a]      = error "oh"
q2 (a:x:[]) = a
q2 (x:xs)   = q2 xs


-- 3.
--


-- 4.
--

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
