module AbstractAlgebra where

skip' n xs = map fst (filter (\x -> snd x `mod` n == 0) (zip xs [1..]))

f = undefined
skip n xs = filter  (pred n) (zip2 xs)
  where

  pred :: Int -> (Int, Int) -> Bool
  pred = flip (((==5) .) . (mod . snd))

  zip2 :: [Int] -> [(Int, Int)]
  zip2 = flip zip [1..]
