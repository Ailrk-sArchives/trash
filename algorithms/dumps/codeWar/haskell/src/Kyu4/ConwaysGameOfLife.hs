module Kyu4.ConwaysGameOfLife where

import           Data.List
import           Data.Maybe

getGeneration :: [[Int]] -> Int -> [[Int]]
getGeneration start gen = foldl (.) id (replicate gen step) $ start

step = trim . play . expand

expand :: [[Int]] -> [[Int]]
expand xss = margin ++ ((\xs -> [0] ++ xs ++ [0]) <$> xss) ++ margin where
  margin = [replicate ((+2) . length . fromMaybe [] $ xss !? 0) 0]

trim :: [[Int]] -> [[Int]]
trim xss = transpose . trim' . transpose . trim' $ xss where
  trim' xss = let t = (length . fromMaybe [] $ xss !? 0)
               in dropWhileTail (== replicate t (0 :: Int))
                . dropWhile ( == replicate t 0) $ xss
  dropWhileTail pred = reverse . (dropWhile pred) . reverse

play :: [[Int]] -> [[Int]]
play xss = (annotate $ fmap annotate xss) >>= \(i, xs) -> return $ do
  (j, x) <- xs
  let overpop = check xss i j > 3; underpop = check xss i j < 2
  return $ case x of
             0 -> if check xss i j == 3 then 1 else 0
             _ -> if overpop || underpop then 0 else 1
 where
   annotate = zipWith (,) [0..]

-- num of adjecent living cells
check :: [[Int]] -> Int -> Int -> Int
check xss i j = sum . fmap (fromMaybe 0)
    $ [idx i' j' | i' <- [i-1..i+1] , j' <- [j-1..j+1], (i', j') /= (i, j)]
  where
    idx i j = xss !? i >>= \ys -> ys !? j >>= \e -> return e

xs !? i = if i < length xs && i >= 0 then Just (xs !! i) else Nothing
