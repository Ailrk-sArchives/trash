module Other.FAlgebra where

-- https://www.schoolofhaskell.com/user/bartosz/understanding-algebras

-- The minial representation of algebra.

{-@ @-}


-- a functor, a type, and a function => an algebra
-- you can:
-- 1. form an expresion
-- 2. evaluate an expression

type Algebra f a = f a -> a




