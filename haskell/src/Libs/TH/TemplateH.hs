{-# LANGUAGE GADTs           #-}
{-# LANGUAGE LambdaCase      #-}
{-# LANGUAGE TemplateHaskell #-}

module Libs.TH.TemplateH where

import           Control.Monad
import           Language.Haskell.TH
import           Libs.TH.QQHtml


{-@ play with template haskell
    template function are also called meta functions.
    the code they generate is called object functions.

    In template haskell you can manipulate AST elements, constructing
    new AST with their constructors.

    For instance, you can create an identity function by writing
      do
        x <- newName "x"
        return $ LamE [VarP x] (VarE x)

    This at compile time becomes \x -> x
    ast elements ended with an E means it's a expression. P means a pattern,
    D means a declaration

    We have four main ast types
      Exp, Dec, Pat, Type

@-}
-- it basically works like macro in lisp. Different from
-- c++ templates, you build up AST in haskell templates.

-- a tempalt to generate curry functions. --

-- curryN is defined as a meta program, it will run at the compile time --

curryN :: Int -> Q Exp
curryN n = do
  f <- newName "f"
  xs <- replicateM n (newName "x")
  let args = map VarP (f:xs)
      ntup = TupE (map  (Just . VarE) xs)
  return $ LamE args (AppE (VarE f) ntup)

-- use abstract syntax directly
getCurries1 :: Int -> Q [Dec]
getCurries1 n = traverse mkCurryDec [1..n]
  where
    mkCurryDec ith = do
      curry <- curryN ith
      let name = mkName $ "curry" ++ show ith
      return $ FunD name [Clause [] (NormalB curry) []]

-- use syntax function
-- in this case you don't need to manually bind in Q monad.
getCurries2 :: Int -> Q [Dec]
getCurries2 n = traverse mkCurryDec [1..n]
  where
    mkCurryDec ith = funD name [clause [] (normalB (curryN ith)) []]
      where name = mkName $ "curry" ++ show ith

fstN :: Int -> Q Exp
fstN n = do
  x <- newName "x"
  return $ LamE [TupP $ VarP x : replicate (n - 1) WildP] (VarE x)

-- use syntax bracket
getId :: Q Exp
getId = [| id |]

-- a longer exaple
getTrueCase :: Q Exp
getTrueCase = [| \case
      True -> True
      _    -> error "not true"
  |]

getHOAS :: Q [Dec]
getHOAS =  [d|
  data ExprHOAS a where
    Con :: a -> ExprHOAS a
    Lam :: (ExprHOAS a -> ExprHOAS b) -> ExprHOAS (a -> b)
    App :: ExprHOAS (a -> b) -> ExprHOAS a -> ExprHOAS b

  i :: ExprHOAS (a -> a)
  i = Lam id

  k :: ExprHOAS (a -> b -> a)
  k = Lam (\x -> Lam . const)

  s :: ExprHOAS ((a -> b -> c) -> (a -> b) -> (a -> c))
  s = Lam (\x -> Lam (\y -> Lam (\z -> App (App x z) (App y z))))

  skk = App (App s k) k

  eval :: ExprHOAS a -> a
  eval (Con v)     = v
  eval (Lam f)     = \x -> eval . f . Con
  eval (App e1 e2) = eval e1 (eval e2)
  |]
