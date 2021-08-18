{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module NaiveCPS where

-- Naive cps converter
import           Control.Monad.Trans.State
import           Data.Char                    (isDigit, isLetter, isSpace)
import           Data.Functor.Identity        (Identity (Identity))
import qualified Text.ParserCombinators.ReadP as P

-- CPS transformation:
-- before:
--  (g a) halt
--
-- after
--  ((\k1. \k2. k1 k2 halt) a) g

type Var = String
data Expr = Lam { param :: String,  body :: Expr }
          | Var Var
          | App { lam :: Expr, arg :: Expr }
          deriving (Show)

-- target cps
-- cps has two kinds of experssions: atomic and complex.
-- atomic expr always pure and reduce to a value
-- complex expr may not terminate, or may have side effects.
-- data AExpr = ALam (Var, Var) CExpr
--            | AVar Var
-- data CExpr = CExpr AExpr AExpr

-------------------------------------------------------------------------------
type LC a = StateT Int Identity a
uniqueId :: LC Int
uniqueId = get >>= \s -> modify (\s -> s + 1) >> get

uniqueSym :: LC String
uniqueSym = uniqueId >>= \s -> pure $ "k" ++ show s

-------------------------------------------------------------------------------
-- Naive cps transformation

-- | convert an atomic value into CPS value
--  a  => \k. k a
transformM :: Expr -> LC Expr
transformM  = undefined

-- | take an expression and a continuation,
--   apply the continuation  cps converted version of the expression
transformT :: Expr -> Expr -> LC Expr
transformT = undefined

