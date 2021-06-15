module Kyu6.BuildAPileOfCubesSpec where

import Kyu6.BuildAPileOfCubes
import Data.Foldable (for_)
import Data.List (permutations)
import System.Random
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Text.Printf (printf)



spec :: Spec
spec = do
  describe "Solution" $ do
    testFindNb 4183059834009 2022
    testFindNb 24723578342962 ((-1))
    testFindNb 135440716410000 4824
    testFindNb 40539911473216 3568
  where
    testFindNb :: Integer -> Integer -> Spec
    testFindNb m r =
      it (printf "m %i " m) $
        findNb m `shouldBe` r
