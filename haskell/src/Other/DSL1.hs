{-# LANGUAGE GADTs              #-}
{-# LANGUAGE RankNTypes         #-}
{-# LANGUAGE StandaloneDeriving #-}

module Other.DSL1 where

import           Prelude hiding (succ)

-- untyped typed lambda calculus with dsl
-- basically it's partial evalution with defuntionalization.

-- We shove programs into dataype so they are defunctionalized and properly
-- typed for specific problem. This allows us to create a program without
-- evluating it, and it brings some benefits:
--  1. we can write program to build up programs.
--  2. we can shove in useful information when building up the program.
--  3. we can control when to evaluate the program.
--  4. we can write different combinators that supports different features,
--     all compile to the same data type, so single source of truth.
--
-- The dsl has it's original semantics, to restore it, we define an evaluation
-- function that actually evaluate the constructed data type.
--
-- To some extend it's like the reverse of macro, instead of handling all
-- program with macro, we define a small language with limited functionalities,
-- and handle the construction of the program as normal datat type.
-- Not like macro, we need to manually convert the program back to haskell to
-- run it.

data LCExpr a where
  Const :: a -> LCExpr a
  Abst :: (LCExpr a -> LCExpr b) -> LCExpr (a -> b)
  App :: LCExpr (a -> b) -> LCExpr a -> LCExpr b

-- instance Functor LCExpr where
--   fmap f (Const a) = Const (f a)
--   fmap f (Abst l) = Const (\a -> (l (Const a)) _)

-- A eval functions that restores the original semantic of the program.
eval :: LCExpr a -> a
eval (Const a) = a
eval (Abst f)  = eval . f . Const
eval (App f e) = eval f (eval e)

showConst :: Show a => LCExpr a -> String
showConst (Const a) = show a
showConst _         = error "can't show"

-- constant
-- prop> eval (Const 1) == 1
-- +++ OK, passed 1 test.

-- apply the abstraction
-- >>> let f = (Abst (\x -> Const $ eval x + 1))
-- prop> eval (App f (Const 1)) == 2
-- +++ OK, passed 1 test.

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

-- >>> let f = (lambda (\x -> val $ eval x + 1))
-- prop> (eval $ apply f (val 3)) == 4
-- +++ OK, passed 1 test.

-------------------------------------------------------------------------------
-- church encoding

-- note the LCExpr type essentially defunctionalized lambda application so
-- we have a data type that represents lambdas to work with.
-- All other operations are based on partial evaluates to the data type and
-- do some analysis.

z :: LCExpr (f -> x -> x)
z = lambda $ \f -> lambda $ \x -> x

succ =
  lambda $ \n ->
  lambda $ \f ->
  lambda $ \x ->
    val $ eval f (eval n $ (eval f) (eval x))

  -- lambda $ \n ->
  -- lambda $ \f ->
  -- lambda $ \x ->
  --   (eval f) (eval n (eval f) (eval x))

plus n =
  lambda $ \m ->
  lambda $ \n ->
  lambda $ \f ->
  lambda $ \x ->
    (eval m) (eval f) ((eval n) (eval f) (eval x))

num n = lambda $ \f ->
  lambda $ \x ->
    let f' = eval f
     in foldr (.) f' (replicate n f') (eval x)

mult n =
  lambda $ \m ->
  lambda $ \n ->
  lambda $ \f ->
  lambda $ \x ->
    (eval m) (eval n (eval f)) (eval x)

-- >>> let one = succ z
-- Couldn't match expected type ‘LCExpr (a0 -> LCExpr b1 -> b1) -> t’
--             with actual type ‘LCExpr
--                                 (((t10 -> LCExpr b0) -> t20 -> t10)
--                                  -> (t10 -> LCExpr b0) -> t20 -> b0)’


-------------------------------------------------------------------------------
-- scott encoding
