{-# LANGUAGE TemplateHaskell #-}
module Kyu4.TemplateHaskellTuplemaker where

import Language.Haskell.TH

-- | Creates a lambda that takes `n` arguments and
-- | returns an n-tuple of those arguments.
tuple :: Int -> Q Exp
tuple 0 = undefined
tuple 1 = undefined
tuple n = undefined
