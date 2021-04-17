module PolynomialRing where

-- polynomial ring.

class Monoid r => Group r where
  invA :: r -> r -- additive inverse

class (Monoid r) => Ring r where
  (<@>) :: r -> r -> r
  rempty :: r -- 1

class Ring r => CommutativeRing r

class CommutativeRing r => IntegralDomain r

class IntegralDomain r => PrincipleIdealDomain r

class IntegralDomain r => UniqueFactorizationDomain r where
  isIrreducible :: r -> Bool
  factorize :: r -> [r]

class (IntegralDomain r, UniqueFactorizationDomain r, PrincipleIdealDomain r) => EuclideanDomain r where
  euclidean :: r -> r -> r

data Bin = O | I deriving (Show, Enum)

type Polynomial r = ([r], [r])

pow :: Ring r => r -> Integer -> r
pow r 0 = r
pow r n = pow (r <@> r) (n - 1)

-----------------------------------------------------------
-- define polynomial as dot product
-- [a b c] [1 0 1]
polynomial :: (Eq r, Ring r) => [r] -> [r] -> Polynomial r
polynomial _ xs | any (\x -> x /= mempty || x /= rempty) xs = error "wrong ploy"
polynomial as xs = (as, xs)

-- to evaluate a polynomial.
-- p(x) = ao + a1x + a2x^2 + a3x^3 ... + anx^n
eval :: Ring r => Polynomial r -> r
eval (as, xs) = mconcat (zipWith3 term as xs [0 ..])
  where
    term :: Ring r => r -> r -> Integer -> r
    term a x d = a <@> pow x d

-- We can prove if R is a ring, then it's polynomial ring R[x] is also a ring.
-- simply write out the full form, assume p(x) and q(x) are not zero polynomial, and
-- see if tehre product are non zero polynomial.

-----------------------------------------------------------
-- Notice,
-- the idea of irreducible elements in an integral domain means x = yz where either
-- y or z is an unit.
-- where unit is an element in ring R with multiplicative inverse. that is, ax = 1
--
-- Note, in polynomial rings, if g(x) = p(x)q(x)
--  deg(g(x)) = deg(p(x)) + deg(q(x))
-- So the only possible factors for irreducible polynomial is itself and the constant
-- polynomial.
--
-- also because we can always factoring out the constant polynomial, we can assume
-- poynomial are monic, that is, the leading coefficient is 1.

-----------------------------------------------------------
-- What are values in a polynomial ring?
-- 1. constant are polynomials too
-- 2. x is a polynomial
-- 3. 0 is the zero polynomial

-----------------------------------------------------------
-- if for R[x], R is a field, call it F with identity 1.
-- then units of F[x] can only be constant polynomial, because only constant polynomial
-- will multiple to yield one.
--
-- because for a field all non zero elements are units, so for F[x] all non constant
-- polynomials are unit.
-- Or in another word, polynomials with degree larger than one can't be unit
--
-- That means, the only possible factors of an irreducible polynomial are non zero constants.
-- e.g (x^2 - 2) is irreducible over Q[x] = 1 . (x^2 - 2)

-----------------------------------------------------------
-- concept:
-- A polynomial f(x) has a linear (degree 1) factor (x - a) iff p(a) = 0.

-----------------------------------------------------------
-- ring of integer Z and Polynomial over field F F[x] --
-- Ring of integer: Z  and
-- polynomial over field F F[x]
-- has similar algebraic properties.
--
--    Z                               F[x]
--  - they are both integral domain and euclidean domains.
--  - so no zero divisor, we can define division on them.
--  Z is integral domain           f[x] is integral domain
--  and euclidean domain           and euclidean domain
--
--  - division algorithm works (corollary from the first one.)
--  a = bq + r o <= r < |b|        a(x) = b(x)q(x) + r(x), 0 <= deg(r(x)) < deg(b(x))
--
-- - units
-- U(Z) = {1, -1}                  U(F[x]) = F* = {nonzero constant polynomials}
--
-- - ring of x mod n
-- Zn = Z/nZ ring of integer mod n F[x]/(f(x)) = ring of polynomial mod n
--
-- - prime and irreducible makes a field
-- p prime <=> Zp is a field       f(x) irreducible <=> F[x]/(f(x))l is irreducible.


-----------------------------------------------------------
-- generator polynomial and ideal

-----------------------------------------------------------
-- every euclidean domain is a principle ideal domain.
-- meaning, all ideals has a generator polynomial
-- meaning, exists a generator polynomial that repeately multiple itself generate
--          the entire ideal
