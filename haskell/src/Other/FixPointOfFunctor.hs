{-# LANGUAGE DeriveFunctor #-}

module Other.FixPointOfFunctor where

import           Control.Arrow

-- Fix Point Of Functor


data Lit
  = StrLit String
  | IntLit Int
  | Ident String
  deriving (Show, Eq)

data Expr a
  = Index a a
  | Call a [a]
  | Unary String a
  | Binary a String a
  | Paren a
  | Literal Lit
  deriving (Show, Eq, Functor)


-- use a fix point to tight the recursive knot.
data Fix f = In (f (Fix f))

-- out is a helper function to evaluate the Term f
out :: Fix f -> f (Fix f)
out (In t) = t


n = let term = In (Binary (In (Paren (In (Literal (IntLit 3)))))
                           "+"
                           (In (Binary (In (Binary (In (Literal (IntLit 3)))
                                           "*"
                                           (In (Literal (IntLit 10)))))
                                        "-"
                                        (In (Literal (IntLit 10))))))
      in out term
