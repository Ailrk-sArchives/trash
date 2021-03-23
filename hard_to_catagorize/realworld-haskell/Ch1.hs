module Ch1 (
  wc
  ) where

-- 1.

wc = interact wordCount
  where
    wordCount input = show (length $ lines input) ++ "\n"
