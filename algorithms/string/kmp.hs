module Kmp1 where

import           Data.Array (Array, bounds, listArray, (!))

-- Knuth Morris Pratt algorithm.
{-@ Motivation:
  A brute force searching for the position of a substring pat
  in a given string txt has time complexity O(k.n) where k is
  the length of pat, n is the length of the text.
  If k and n are close then basically you have O(n^2).

  KMP has two stages: build jump table and match.
  building the table is O(k), and matching is O(n). Thus
  the final worse case complexity is O(k) + O(n), this can
  be considered as linear.

  Jump table is also called longest prefix suffix (lps). It
  stores the lenght of the maximum proper prefix that is also
  a sufix. (can't be the entire string)

  Jump table of s ::= {x | x is proper prefix of s &&
                           x is proper suffix of s   }
@-}
-- e.g
-- Jump table of string pat = "abxab":
--  a  b  x  a  b
--  0  0  0  1  2
--
-- Jump table of string pat = "aaabcaa":
--  a  a  a  b  c  a  a
--  0  0  0  0  0  1  2

data Table a = Table
 { word      :: Array Int a
 , jumpTable :: Array Int Int
 , len       :: Int
 }

type MatchState = Int

-- build kmp table O(n)
build :: Eq a => [a] -> Table a
build pattern = table
  where
    len = length pattern
    table = Table
      { word      = listArray (0, len - 1) pattern
      , jumpTable = listArray (-1, len - 1) $ (-2) : genJump (-1) 0
      , len       = len
      }

    pred a b c =  len == a ||
              word table ! b /= word table ! c

    genJump _ 0 =
      let o = if pred 1 0 1 then -1 else -2
          later = genJump (-1) 1
       in o : later

    genJump lastMPJump i =
      let ch = word table ! i
          findJ j
            | j == -2 = -2
            | word table ! (j + 1) == ch = j
            | j == -1 = -2
            | otherwise = findJ (jumpTable table ! j)
          j = findJ lastMPJump

          o = if pred (i + 1) (i + 1) (j + 2)
                 then j + 1
                 else jumpTable table ! (j + 1)

          later  = genJump (j+1) (i+1)
       in o:later

matchOne :: Eq a => Table a -> MatchState -> a -> (Bool, MatchState)
matchOne table j s
  | hasFind  = (j + 1 == len table, j + 1)
  | otherwise = matchOne table (1 + (jumpTable table ! (j - 1))) s
  where
    hasFind = j < 0 || j < len table && s == word table ! j

match :: Eq a => Table a -> [a] -> [Int]
match table str = [ 0 | len table == 0 ] ++ go (1 - len table) 0 str
  where
        go _ _ [] = []
        go i j (x:xs) = case matchOne table j x of
                             (False, j') -> go (i + 1) j' xs
                             (True, j')  -> i : go (i + 1) j' xs

-- with the jump table created, we can start to match for pattern.
-- still use "abxab" as an example, the jump table is
--  |a b x a b
--  |0 0 0 1 2
--  case:
--     |abxababxab
--     |abxab       -> [0]
--    1. the first match matched unitl index 4, so we look up the jump
--       table [4] to know how many characters to jump.
--    2. In this case is 2, this means we should align pat[2]
--       with the next character
--     |abxababxab
--     |   abxab
--    3. Try to match again, now we match successfully until pat[1].
--       jumptable[1] = 0, so align pat[0] with the next char.
--     |abxababxab
--     |     abxab  -> [0, 5]
--    4. Find another match, end.
