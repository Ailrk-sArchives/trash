{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module HybridCPS where

-- Naive cps converter
import           Control.Monad.Trans.State
import           Data.Char                    (isDigit, isLetter, isSpace)
import qualified Text.ParserCombinators.ReadP as P


