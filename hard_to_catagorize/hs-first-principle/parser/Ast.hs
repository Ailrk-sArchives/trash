module Ast where

-- for simple calculator
data Expr
  = Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Lit Int
  deriving (Show)

eval :: Expr -> Int
eval (Lit n) = n
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b
eval (Sub a b) = eval a - eval b
