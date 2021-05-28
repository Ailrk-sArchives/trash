{-# LANGUAGE KindSignatures    #-}
{-# LANGUAGE OverloadedStrings #-}

module Other.FinalTagless2 where

-- use typeclass to chose the return type.

import           Data.Text.Lazy.Builder


data ExprInterp a = Expr { unExpr :: a }


class Expr (m :: * -> *) where
  bool :: Bool -> m Bool
  int :: Int -> m Int
  leq :: m Int -> m Int -> m Bool
  and' :: m Bool -> m Bool -> m Bool
  or' :: m Bool -> m Bool -> m Bool
  not' :: m Bool -> m Bool


instance Expr ExprInterp where
  bool b = Expr b
  int i = Expr i
  leq (Expr x) (Expr y) = bool (x <= y)
  and' (Expr x) (Expr y) = bool (x && y)
  or' (Expr x) (Expr y) = bool (x || y)
  not' = Expr . not . unExpr


expr1 = unExpr $ and' (or' (bool True) (leq (int 2) (int 3))) (bool True)



-- another implementation

data PersistValue
  = PersistInt64 Int
  | PersistBool Bool
  deriving Show

data PersistExpr a = PersistExpr { unPersistExpr :: (Builder, [PersistValue]) }

instance Expr PersistExpr where
  bool b = PersistExpr ("?", [PersistBool b])
  int i = PersistExpr ("?", [PersistInt64 i])
  leq (PersistExpr (x1, v1)) (PersistExpr (x2, v2)) = PersistExpr (x1 <> " <= " <> x2, v1 <> v2)
  or' (PersistExpr (x1, v1)) (PersistExpr (x2, v2)) = PersistExpr (x1 <> " OR " <> x2, v1 <> v2)
  and' (PersistExpr (x1, v1)) (PersistExpr (x2, v2)) = PersistExpr (x1 <> " AND " <> x2, v1 <> v2)
  not' (PersistExpr (x, v)) = PersistExpr ("NOT " <> x, v)

expr2 = unPersistExpr $ and' (or' (bool True) (leq (int 2) (int 3))) (bool True)
