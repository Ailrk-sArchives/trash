module Kmp where

import Data.Sequence


data JumpTable a = JumpTable
             { pattern :: String
             , lsp     :: [Int]
             , size    :: Int
             }
