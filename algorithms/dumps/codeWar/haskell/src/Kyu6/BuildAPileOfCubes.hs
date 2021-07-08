-- <Build a pile of Cubes>

-- Your task is to construct a building which will be a pile of n cubes.
-- The cube at the bottom will have a volume of n^3, the cube above will
--  have volume of (n-1)^3 and so on until the top which will have a volume of 1^3.

-- You are given the total volume m of the building. Being given m can you find the
--  number n of cubes you will have to build?

-- The parameter of the function findNb (find_nb, find-nb, findNb) will be an
--  integer m and you have to return the integer n such
-- as n^3 + (n-1)^3 + ... + 1^3 = m if such a n exists or -1 if there is no such n.
module Kyu6.BuildAPileOfCubes where

import           Data.Foldable         (for_)
import           Data.List             (permutations)
import           System.Random
import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           Text.Printf           (printf)



----------------------------------------
-- first attempt
----------------------------------------
-- This solution gives timeout. Need to optimize it.
-- [TODO: In cpp version recurison throws segfault after calling large number.]
findNb' :: Integer -> Integer
findNb' = f 0 1
  where
    f :: Integer -> Integer -> Integer -> Integer
    f _ _ 0 = 0
    f acc n m
      | acc > m = -1
      | acc == m = n - 1
      | otherwise = f (acc + n ^3) (n+1) m

----------------------------------------
-- second attempt
----------------------------------------
-- notice m = ( n(n+1) / 2 )^2 = ∑n^3
-- n = ⌊√( ⌊√(m)⌋ * 2)⌋
-- This is the correct approach.
-- all other solutions are some variations
-- of doing math.

findNb :: Integer -> Integer
findNb m = if m == m' then r else -1
  where fsqrt = floor . sqrt . fromIntegral
        r = fsqrt (fsqrt m * 2)
        m' = div (r * (r + 1)) 2 ^ 2

-- testing --------------------------------------------------------------------


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
