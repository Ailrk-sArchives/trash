module Kyu6.HexagonBeamMaxSum where

import Debug.Trace
import Prelude hiding ((!!))
import qualified Prelude ((!!))

data Hex = Hex
  { hn :: Int,
    hseq :: [Int]
  }

maxHexagonBeam :: Int -> [Int] -> Int
maxHexagonBeam n lst = maximum $ f <$> transforms
  where
    f = maxsum $ Hex n lst
    transforms =
      [ \hex r c -> (r, c),
        rotateLeftTrans,
        rotateRightTrans
      ]

type CoordTransform = Hex -> Int -> Int -> (Int, Int)

rotateLeftTrans :: CoordTransform
rotateLeftTrans hex r c
  | r < n = if c < n then (c, r) else (c, n - 1 - gap)
  | c < n - 1 =
    ( revGap + revIdx,
      if r + c >= rowSz hex
        then r - c + ((rowSz hex) - r - 1)
        else r
    )
  | c == n - 1 = (r, c)
  | otherwise = (r + gap, (colSz hex r) - c + revIdx)
  where
    (_, pc) = if r < n then (r, r) else (r, n - 1)
    n = hn hex
    gap = abs $ pc - c
    revIdx = r - n
    revGap = (abs $ n - gap)

rotateRightTrans :: CoordTransform
rotateRightTrans hex r c =
  let (r', c') = rotateLeftTrans hex r c
      upperBound = rowSz hex - 1
   in (upperBound - r', c')

maxsum :: Hex -> CoordTransform -> Int
maxsum hex tranform =
  traceShow hexCoordTable $
  maximum . concat $ (fmap . fmap) getVal hexCoordTable
  where
    getVal (r, c) = hex !! tranform hex r c
    hexCoordTable = do
      r' <- [0 .. rowSz hex]
      return [(r', c') | c' <- [0 .. colSz hex r']]

(!!) :: Hex -> (Int, Int) -> Int
hex !! pos = hseq hex Prelude.!! (traceShow (getIdx hex pos) $ (getIdx hex pos))

getIdx :: Hex -> (Int, Int) -> Int
hex `getIdx` pos = pre + col `mod` (length . hseq) hex
  where
    (row, col) = pos
    pre = sum $ colSz hex <$> [0 .. row]

rowSz :: Hex -> Int
rowSz hex = hn hex * 2 - 1

colSz :: Hex -> Int -> Int
colSz hex row
  | row <= n = n + row
  | otherwise = (rowSz hex) - (row + 1 - n)
  where
    n = hn hex
