module MiniProject.PythagoreanNTuple where

-- three ways to categorize a number
-- A: even numbers which is only powers of 2.
-- B: odd numbers consists of powers of any primes.
-- C: even numbers with both power of m and other primes.
import Data.List
import Control.Applicative
import Data.Maybe
import Control.Monad

-- helper functions.
isPrime :: Integer -> Bool
isPrime n =
  (n > 1) && null [ x | x <- [2..floor.sqrt.fromIntegral $ n], n `mod` x == 0]

primeFactors :: Integer -> [Integer]
primeFactors n = factors n 2
  where factors 1 _ = []
        factors n f
          | n `mod` f == 0 = f : factors (n `div` f) f
          | otherwise = factors n (f + 1)

type M = Integer
type R = Integer
type P = Integer
type S = Integer
type K = Integer

data Case = A M
          | B P S K  -- T depend on cases.
          | C M P S K
          deriving (Eq, Show)

aDeltaConstraint a delta = a > delta
deltaFactorConstraint a delta = a^2 `mod` delta == 0

-- check which category a lands on and decompose into the corresponding form.
decompose :: Integer -> [Case]
decompose a
  | all (==2) factors = [A (toInteger $ length factors)] -- a = 2^m, ∆ = 2^r where r = 1
  | 2 `notElem` factors =                                -- a = p^s * k, ∆ = p^t
    let ps = getps factors                               -- where t = 0 or t = 2s
     in (\(p, s) -> B p s (a `div` p^s)) <$> ps
  | otherwise =                                          -- a = 2^m * p^s, ∆ = 2^r*p^t
    let oddFactors = filter odd factors
        evenFacors = filter even factors
        ps = getps oddFactors
        m = toInteger . length $ evenFacors
     in (\(p, s) -> C m p s (a `div` (p^s * 2 * m))) <$> ps
  where factors = primeFactors a
        getps fs = let l = group fs in
          zip (head <$> l) (fromIntegral . length <$> l)


check a = liftA2 (&&) (aDeltaConstraint a) (deltaFactorConstraint a)

-- calculate delta by given cases
getdelta :: Case -> [Maybe Integer]
getdelta (A _) = [Just 2]
getdelta (B p s k) =
  let d1 = p^(2*s)
      a = p ^ s * k
   in if check a d1
         then Just <$> [d1, 1]
      else  [Just 1]
getdelta (C m p s k) =
  let dmGTr1 = 2                        -- if m > r, r = 1, t = 0 or t = 2s
      dmGTr2 = 2 * (p ^ (2*s))
      dmLTr1 = 2 ^ (2*m-1)              -- if m < r, r = 2m - 1, t = 0 or t = 2s
      dmLTr2 = 2 ^ (2*m-1) * p ^ (2*s)
      a =  p ^ s * k * 2^m
   in Just <$> filter (check a) (nub [dmGTr1, dmGTr2, dmLTr1, dmLTr2])



getdeltas :: Integer -> [Maybe Integer]
getdeltas a = nub $ getdelta =<< decompose a


getTrple :: Integer -> Maybe [(Integer, Integer, Integer)]
getTrple a = if a <=2 then Nothing else triple

  where deltas = fromMaybe [] (sequenceA $ filter (/=Nothing) (getdeltas a))
        b de = (a^2 - de^2) `div` (2*de)
        bs = b <$> deltas
        cs = zipWith (+) bs deltas
        triple = Just $ zipWith ((,,) a) bs cs
