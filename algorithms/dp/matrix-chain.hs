module MatrixChain where

import Control.Monad
import Control.Monad.RWS

-- replication of the lisp version

data Matrix = M [[Int]]

instance Show Matrix where
  show (M []) = "\n"
  show (M (n:ns)) = (case n of
                      [] -> "[]"
                      xs -> show xs)
                 <> "\n"
                 <> show (M ns)

eye :: (Int, Int) -> Matrix
eye (m, n) = M [ [0 | _ <- [1..n]] | _ <- [1..m] ]

dim :: Matrix -> (Int, Int)
dim (M []) = (0, 0)
dim (M ms)
  | all (== n) (fmap length ms) = (length ms, n)
  | otherwise = error "wrong matrix shape"
  where
    n = length . head $ ms

mmult :: Matrix -> Matrix -> Matrix
mmult mat1 mat2
  | n1 /= m2 = error "Matrix can't be multiplied"
  | otherwise = undefined
  where
    (m1, n1) = dim mat1
    (m2, n2) = dim mat2

cost :: Matrix -> Int
cost mat = let (m, n) = dim mat
            in m * n

parenthesization :: Matrix -> Int
parenthesization = undefined
