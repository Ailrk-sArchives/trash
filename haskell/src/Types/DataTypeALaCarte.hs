{-# LANGUAGE TypeOperators #-}

module Types.DataTypeALaCarte where

-- http://www.cs.ru.nl/~W.Swierstra/Publications/DataTypesALaCarte.pdf

-- assembling both data types and functions from isolated individual components
-- a way to solve the expression problem.

-- adt has the opposite problem of subtyping --
data Expr' = Val' Int | Add' Expr' Expr'
-- to add a type constructor we need to add a corresponding case for every functions.

eval :: Expr' -> Int
eval (Val' x)   = x
eval (Add' x y) = eval x + eval y

render :: Expr' -> String
render (Val' x)   = show x
render (Add' x y) = show (eval x + eval y)

{-@ data a la carte @-}

-- use coproduct to combine two data constructors.
data (f :+: g) e = Inl (f e) | Inr (g e)

data Expr f = In (f (Expr f))

data Val e = Val Int
data Add e = Add e e

-- add example
addEx :: Expr (Val :+: Add)
addEx = In (Inr (Add
                  (In (Inl (Val 188)))
                  (In (Inl (Val 1219)))))

-- but how do you evaluate this thing??

-- first realize all data types above are functors.
instance Functor Val where
  fmap _ (Val x) = Val x

instance Functor Add where
  fmap f (Add e1 e2) = Add (f e1) (f e2)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap f (Inl e1) = Inl (fmap f e1)
  fmap f (Inr e2) = Inr (fmap f e2)

-- because they are all functors, we can fold an Expr as follow:
foldExpr :: Functor f => (f a -> a) -> Expr f -> a
foldExpr f (In t) = f (fmap (foldExpr f) t)
