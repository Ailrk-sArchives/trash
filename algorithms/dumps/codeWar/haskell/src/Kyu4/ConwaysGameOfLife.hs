module Kyu4.ConwaysGameOfLife where

import           Data.List
import           Data.Maybe
import           Test.HUnit
import           Test.Hspec


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


-- testing --------------------------------------------------------------------
gliders = [[[1,0,0],
            [0,1,1],
            [1,1,0]],
           [[0,1,0],
            [0,0,1],
            [1,1,1]],
           [[1,0,1],
            [0,1,1],
            [0,1,0]],
           [[0,0,1],
            [1,0,1],
            [0,1,1]]
          ]

twoGliders = [[[1,1,1,0,0,0,1,0],
               [1,0,0,0,0,0,0,1],
               [0,1,0,0,0,1,1,1]],
              [[1,1,1,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [1,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,1,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,0],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1],
               [0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1]]
             ]

fPentomino = [[0, 1, 0], [0, 1, 1], [1, 1, 0]]

main = hspec spec
spec = do
  describe ("Glider\n" ++ htmlize (gliders !! 0)) $ do
    it "Glider 0" $ do
      assertLife (gliders !! 0) 0 (gliders !! 0)
    it "Glider 1" $ do
      assertLife (gliders !! 0) 1 (gliders !! 1)
    it "Glider 2" $ do
      assertLife (gliders !! 0) 2 (gliders !! 2)
    it "Glider 3" $ do
      assertLife (gliders !! 0) 3 (gliders !! 3)
    it "Glider 40" $ do
      assertLife (gliders !! 0) 40 (gliders !! 0)

  describe ("Two Gliders\n" ++ htmlize (twoGliders !! 0)) $ do
    it "Two Gliders 100" $ do
      assertLife (twoGliders !! 0) 16 (twoGliders !! 1)

assertLife start gen expected = do
    let actual = getGeneration start gen
        errorMsg = intercalate "\n" ["expected:", htmlize expected, "got:" , htmlize actual]

    assertBool errorMsg (actual == expected)


htmlize :: [[Int]] -> String
htmlize = concatMap ((++ "\n") . concatMap cell)
  where cell n = if n > 0 then "#" else "0"
