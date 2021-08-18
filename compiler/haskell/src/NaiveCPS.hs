{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module NaiveCPS where

-- Naive cps converter
import           Control.Monad.IO.Class
import           Control.Monad.Trans          (lift)
import           Control.Monad.Trans.Maybe
import           Data.Char                    (isDigit, isLetter, isSpace)
import           Data.Functor.Identity        (Identity (Identity))
import           Data.Monoid
import           Data.Text                    as T
import           Data.Unique
import qualified Text.ParserCombinators.ReadP as P

-- CPS transformation:
-- before:
--  (g a) halt
--
-- after
--  ((\k1. \k2. k1 k2 halt) a) g

-- src. just unyped lambda calculus.
data Expr = Lam Text Expr
          | Var Text
          | App Expr Expr
          deriving Show

-- target cps
-- cps has two kinds of experssions: atomic and complex.
-- atomic expr always pure and reduce to a value
-- complex expr may not terminate, or may have side effects.

data AExpr
  = AVar Text
  | ALam [Text] CExpr

data CExpr = CApp AExpr [AExpr]

-------------------------------------------------------------------------------
gensym :: IO Text
gensym = fmap (\u -> T.pack ("#" <> show (hashUnique u))) newUnique

-------------------------------------------------------------------------------
-- Naive cps transformation

transfromM :: Expr -> MaybeT IO AExpr
transfromM expr@(Lam var cexpr) = do
  k' <- liftIO gensym
  cexpr' <- transfromT cexpr (AVar k')
  return $ ALam [var, k'] cexpr'
transfromM expr@(Var var) = return $ AVar var
transfromM _ = MaybeT (return Nothing)  -- you can't lift Nothing...

transfromT :: Expr -> AExpr -> MaybeT IO CExpr
transfromT expr k =
  case expr of
    Lam _ _ -> (CApp k) <$> (fmap (:[]) $ transfromM expr)
    Var _ ->(CApp k) <$> (fmap (:[]) $ transfromM expr)
    App f e -> do
      f' <- liftIO gensym
      e' <- liftIO gensym
      let aexpr = ALam [e'] (CApp (AVar f') [AVar e', k])
      cexpr <- transfromT e aexpr
      let aexpr' = ALam [f'] cexpr
      transfromT f aexpr'

