module EuclideanAlgorithm where

-- https://www.math.cmu.edu/~bkell/21110-2010s/extended-euclidean.html

-- simple euclidean algorithm to compute greatest common divisor
euclideanGCD :: Integer -> Integer -> Integer
euclideanGCD a 0 = a
euclideanGCD a b = euclideanGCD b (a `mod` b)

-- a and b are coprime if gcd(a, b) = 1
isCoprime :: Integer -> Integer -> Bool
isCoprime a b = 1 == euclideanGCD a b

-- a ring with no zero divisor
-- all fields are integral domain
class IntegralDomain r

-- An euclidean domain needs to be an integral domain first.
class (IntegralDomain r) => EuclideanRing r where
  gcd :: r -> r -> r

-- bezouts theorem
-- gcd(a, b) = xa + yb
-- gcd can be written as linear combination of a and b.

-- use bezouts theorem and extended euclidean algorithm to solve
-- multiplicative inverse of integer modulo n.

-- first see a simple modulo arithmetics:
-- a ≡ b (mod n)
-- so a - b = nx
-- so a = nx + b
-- or p = nq + r
-- in another word, n | a - b
data Congruence = Cong Integer Integer Integer
  deriving (Show)

congruence :: Integer -> Integer -> Integer -> Maybe Congruence
congruence a b n
  | (a - b) `mod` n == 0 = Just (Cong a b n)
  | otherwise = Nothing

cong1 = congruence 4 1 3

-- and if a is a product of two numbers, we have
-- ax ≡ b (mod n)
-- so ax - b = ny
-- so b = ax - ny
-- written as b = ax + ny

-- extended euclidean algorithm:
--  if we keep track of more information of the naive euclidean algorithm, we can
--  compute the gcd as an integer linear combination of two numbers.
--    gcd(a, b) = n = xa + yb
--  for example you have gcd(a, b) =

-- When gcd(a, b) = 1, or a, b are coprime, we have
-- 1 = xa + yb.
--
-- given a, we want to find a inverse:
--   a . a¯¹ ≡ 1 (mod b)
-- ⇔ a . a¯¹  ≡ 1 (mod b)
-- ⇔ 1 = a.a¯¹ + by
-- a, b are known, but a¯¹ and y are not known.
-- we want to solve this integer linear combination to get a¯¹.
