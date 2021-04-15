module FiniteField where

-- recall:
-- polynomial ring R[x] is a ring of polynomials with coefficient
-- from R.
--
-- quotient polynomial ring F[x]/(f(x)) is the polynomial modulo
-- f(x).
--
-- if f(x) is irreducible, we have F[x] is a field
-- This makes a analoguos with ring of integer Z.
-- First of all Zn is a ring.
-- if for Zp, p is prime, Zp is a field.

-- recall
-- given ring R, the set of all units is called R*
-- it happen to be a group called multiplicative of units R*.

-- finite fields --
--
-- -- multiplication in a field has inverse for all non zero
-- elements, so (F \ {0}, *) forms a group --
-- -- multiplicative group of a finite field is cyclic, that is
-- it can generate all non zero elements --
--
-- So for field F[x]/(f(x)), exists exists a polynomial x
-- that it's power generate the entire field.
-- If that's the case, we call f(x) a primitive polynomial
-- x, as the primitive element of the

-----------------------------------------------------------
-- what makes Zp[x]/(f(x)) a finite field?
-- 1. p is prime
-- 2. f(x) is irreducible
-- => we can construct a finite field Zp[x]/(f(x))
--
-- It's a set of all polynomial in Zp[x] with degree < deg(f(x))
-- = { a0 + a1x + a2x^2 + ... + an-1x^n-1 | a in Zp}.
-- We have p choices for all n coefficients.
--
-- So |Zp[x]/(f(x))| = p^n

-----------------------------------------------------------
-- for Zp[x]/(f(x)), we need p be a prime and f(x) be irreducible
-- Why do we need 2 constraints here?
--
-- 1. p be prime:
--    so Zp is a field. Then Zp[x] is a field.
-- 2. f(x) be irreducible:
--    If we just work with Zp[x], we can't have the field finite, becase
--    multiplying two polynomials increase the degree.
--    f(x) is used to constrain the degree.
--
--    how to constrain the

-----------------------------------------------------------
-- General facts about finite fields.
--
-- - The size of any finite field is a prime power.
--
-- - all finite field F with p^n elements is isomorphic to Zp[x]/f(x)
--
-- - for every prime p and n >= 1, there exists a field of order p^n
