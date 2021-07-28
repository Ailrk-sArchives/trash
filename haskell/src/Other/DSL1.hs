{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}

module Other.DSL1 where

import Prelude hiding (succ)

-- untyped typed lambda calculus with dsl

data LCExpr a where
  Const :: a -> LCExpr a
  Abst :: (LCExpr a -> LCExpr b) -> LCExpr (a -> b)
  App :: LCExpr (a -> b) -> LCExpr a -> LCExpr b

-- instance Functor LCExpr where
--   fmap f (Const a) = Const (f a)
--   fmap f (Abst l) = Const (\a -> (l (Const a)) _)

eval :: LCExpr a -> a
eval (Const a) = a
eval (Abst f) = eval . f . Const
eval (App f e) = eval f (eval e)

showConst :: Show a => LCExpr a -> String
showConst (Const a) = show a
showConst _ = error "can't show"

-- constant
p1 = Const 1

-- abstraction
p2 = Abst (\x -> Const $ eval x + 1)

-- apply the abstraction
p3 = App p2 (Const 1)

-------------------------------------------------------------------------------
-- lanaguage

class LC m where
  apply :: m (a -> b) -> m a -> m b
  lambda :: (m a -> m b) -> m (a -> b)
  val :: a -> m a

instance LC LCExpr where
  apply = App
  lambda = Abst
  val = Const

p4 = apply (lambda (\x -> val $ eval x + 1)) (val 3)

-------------------------------------------------------------------------------
-- church encoding

z = lambda (const (lambda eval))

succ n = lambda $ \f ->
  lambda $ \x ->
    let f' = eval f
     in foldr (.) f' (replicate n f') (eval x)

num n = succ $ n + 1

plus = lambda $ \m -> lambda $ \n -> lambda $ \f -> lambda $ \x ->
  eval m (eval f) (eval n (eval f) (eval x))
