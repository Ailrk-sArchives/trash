module Ap () where

-- http://www.staff.city.ac.uk/~ross/papers/Applicative.pdf

-- Some common applicative functor with monad. --
-- 1. seqence
-- basic
seqIO :: [IO a] -> IO [a]
seqIO [] = return []
seqIO (x : xs) = do
  c <- x
  cs <- seqIO xs
  return $ c : cs

-- effect only
seqIO_ :: [IO a] -> IO ()
seqIO_ xs = seqIO xs >> return () -- just discard the effect

-- 2. transpose
-- Primitive version
transpose :: [[a]] -> [[a]]
transpose [] = repeat []
transpose (xs : xss) = zipWith (:) xs $ transpose xss

-- with zapp: "zippy application"
repeat' :: a -> [a]
repeat' x = x : repeat x

zapp :: [a -> b] -> [a] -> [b]
zapp (f : fs) (x : xs) = f x : zapp fs xs
zapp _ _ = []

transpose' :: [[a]] -> [[a]]
transpose' [] = repeat' []
transpose' (xs:xss) = repeat (:) `zapp` xs `zapp` transpose xss

--
