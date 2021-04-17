{-# LANGUAGE CPP #-}

module Affinecipher where

import Data.Maybe
import Prelude hiding (gcd)

-- create a ring of integer modular n.
zn :: Integer -> [Integer]
zn n = take (fromInteger n) [0 .. n -1]

type Col = (Integer, Integer, Integer, Integer)

extendedEuclidean' :: Col -> Col -> (Integer, Integer)
extendedEuclidean' (_, _, s, t) (_, 0, _, _) = (s, t)
extendedEuclidean' (_, r, s, t) (q', r', s', t') =
  let c1 = (q', r', s', t')
      c2 =
        let r'' = r - q' * r'
         in (r' `div` r'', r'', s - q' * s', t - q' * t')
   in extendedEuclidean' c1 c2

extendedEuclidean :: Integer -> Integer -> (Integer, Integer)
extendedEuclidean a b = extendedEuclidean' (undefined, a, 1, 0) (a `div` b, b, 0, 1)

gcd :: Integer -> Integer -> Integer
gcd a b =
  let (s, t) = extendedEuclidean a b
   in a * s + b * t

-- we want to find a, b in Zn,  (a, b) = k, gcd(a, n) = 1
-- why do we want that? because in a commutative ring Zn if you have a with
-- gcd(d, n) = 1, it just saying a is an unit with inverse.
-- we don't really care about the gcd = 1 part, but we need a to be invertable.
ekgen :: Integer -> Integer -> Integer -> Maybe (Integer -> Integer)
ekgen a b n
  | gcd a n /= 1 = Nothing
  | otherwise = Just $ \x -> (a * x + b) `mod` n

-- solve y = ax + b
-- x = a'(y - b)
-- first compute inverse
dkgen :: Integer -> Integer -> Integer -> Maybe (Integer -> Integer)
dkgen a b n
  | gcd a n /= 1 = Nothing
  | otherwise = Just $ \y ->
    let (a', _) = extendedEuclidean a n
     in (a' * (y - b)) `mod` n

z26 = zn 26

ek26 = fromJust $ ekgen 11 4 26
dk26 = fromJust $ dkgen 11 4 26

#ifdef TEST

plaintext = [15, 4, 4, 15, 4, 4, 15, 14, 14, 15, 14, 14]
ciphertext = ek26 <$> plaintext
decryptedtext = dk26 <$> ciphertext

#endif

-- how many possible a?
-- recall Zn is a ring, we need gcd(a, n) = 1 means we want a to be
-- an unit.
-- it's just |Zn*| = ϕ(n)

-- cryptanalysis of affine cipher.
-- affine cipher is a linaer cipher, which makes it very vulnerble over plain
-- text attack.
-- if we known several plain text, we can make a system equations and just solve them.
