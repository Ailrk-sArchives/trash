module Sudoku where

-- a simple soduku solver to simulate CPU bound
-- operations.

import Control.Monad
import Data.Array
import Data.List
import Data.Maybe

type Digit = Char

type Square = (Char, Char)

type Unit = [Square]

-- represent grid as array
type Grid = Array Square [Digit]

rows = "ABCDEFGHI"

cols = "123456789"

digits = "123456789"

box = (('A', '1'), ('I', '9'))

-- from my experience when you're using list comphension
-- the order can be completely omitted since it's an expression
-- anyway. The order of predicate might affect the efficiency, but
-- that's very easy to tell.
cross :: String -> String -> [Square]
cross rows cols = [(r, c) | r <- rows, c <- cols]

squares :: [Square]
squares = cross rows cols

-- array :: Ix i => (i, i) -> [(i, e)] -> Array i e
peers :: Array Square [Square]
peers = array box [(s, set (units ! s)) | s <- squares]
  where
    set = nub . concat

unitList :: [Unit]
unitList =
  [cross rows [c] | c <- cols] ++ [cross [r] cols | r <- rows]
    ++ [ cross rs cs
         | rs <- ["ABC", "DEF", "GHI"],
           cs <- ["123", "456", "789"]
       ]

units :: Array Square [Unit]
units =
  array box $
    [ ( s,
        [filter (/= s) u | u <- unitList, s `elem` u]
      )
      | s <- squares
    ]

allPossibilities :: Grid
allPossibilities = array box [(s, digits) | s <- squares]

parseGrid :: String -> Maybe Grid
parseGrid g = do
  regularGrid g
  foldM assign allPossibilities (zip squares g)
  where
    regularGrid :: String -> Maybe String
    regularGrid g =
      if all (`elem` "0.-123456789") g
        then Just g
        else Nothing

assign :: Grid -> (Square, Digit) -> Maybe Grid
assign g (s, d) =
  if d `elem` digits
    then do
      let ds = g ! s
          toDump = delete d ds
      foldM eliminate g (zip (repeat s) toDump)
    else return g

-- (//) Ix i => Array ie -> [(i, e)] -> Array i e
-- delete :: Eq a => [a] -> [a]
eliminate :: Grid -> (Square, Digit) -> Maybe Grid
eliminate g (s, d) =
  let cell = g ! s
   in if d `notElem` cell
        then return g
        else do
          let newCell = delete d cell
              newV = g // [(s, newCell)]
          newV2 <- case newCell of
            [] -> Nothing
            [d'] -> do
              let peersOfS = peers ! s
              foldM eliminate newV (zip peersOfS (repeat d'))
            _ -> return newV
          foldM (locate d) newV2 (units ! s)

locate :: Digit -> Grid -> Unit -> Maybe Grid
locate d g u = case filter ((d `elem`) . (g !)) u of
  [] -> Nothing
  [s] -> assign g (s, d)
  _ -> return g

search :: Grid -> Maybe Grid
search g =
  case [(l, (s, xs)) | (s, xs) <- assocs g, let l = length xs, l /= 1] of
    [] -> return g
    ls -> do
      let (_, (s, ds)) = minimum ls
      msum [assign g (s, d) >>= search | d <- ds]

solve :: String -> Maybe Grid
solve str = do
  grd <- parseGrid str
  search grd

printGrid :: Grid -> IO ()
printGrid = putStrLn . gridToString

-- elems :: Array i e -> [e]
-- use intermideiate results here to separate a complicated
-- expression into smaller expressions.
-- This will be fused so no performance penalty.
gridToString :: Grid -> String
gridToString g =
  let l0 = elems g
      l1 = (\s -> " " ++ s ++ " ") <$> l0
      l2 = (concat . sublist 3) <$> l1
      l3 = sublist 3 l2
      l4 = (concat . intersperse "|") <$> l3
      l5 = (concat . intersperse [line] . sublist 3) l4
   in unlines l5
  where
    sublist n [] = []
    sublist n xs = ys : sublist n zs
      where
        (ys, zs) = splitAt n xs
    line = hyphens ++ "+" ++ hyphens ++ "+" ++ hyphens
    hyphens = replicate 9 '-'
