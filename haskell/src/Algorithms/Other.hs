module Algorithms.Other where

import Control.Monad
import Control.Monad.Trans.Except

-- https://stackoverflow.com/questions/17264360/how-do-i-break-out-of-a-loop-in-haskell

type Set a = [a]

-- the powerset
powerset :: Set a -> Set (Set a)
powerset [] = [[]]
powerset (x : xs) =
  let current = [x : ps | ps <- powerset xs]
      rest = powerset xs
   in current ++ rest

-- loop
loop :: (Monad m) => ExceptT e m a -> m e
loop = fmap (either id id) . runExceptT . forever

quit :: (Monad m) => e -> ExceptT e m r
quit = except . Left

ls :: [Int]
ls = [1, 2, 3, 4]
