{-# LANGUAGE GADTs              #-}
{-# LANGUAGE StandaloneDeriving #-}

module NaiveCPS where

-- Naive cps converter
import           Control.Monad.Trans.State
import           Data.Char                    (isDigit, isLetter, isSpace)
import           Data.Functor.Identity        (Identity (Identity))
import qualified Text.ParserCombinators.ReadP as P

-- CPS transformation:

type Var = String
data Expr = Lam { param :: String,  body :: Expr }
          | Var Var
          | App { lam :: Expr, arg :: Expr }
          deriving (Show)

-- target cps
-- cps has two kinds of experssions: atomic and complex.
-- atomic expr always pure and reduce to a value
-- complex expr may not terminate, or may have side effects.
data AExpr = ALam [Var] CExpr
           | AVar Var
data CExpr = CExpr AExpr [AExpr]

-------------------------------------------------------------------------------
type LC a = StateT Int Identity a
uniqueId :: LC Int
uniqueId = get >>= \s -> modify (\s -> s + 1) >> get

uniqueSym :: LC String
uniqueSym = uniqueId >>= \s -> pure $ "k" ++ show s

-------------------------------------------------------------------------------
-- Naive cps transformation

-- | convert an atomic value into CPS value
transformM :: Expr -> LC AExpr
transformM (Lam param body) = do
  k' <- uniqueSym
  body' <- transformT body k'
  return $ ALam [param, k'] body'
transformM (Var n) = return $ AVar n
transformM _ = error "M tranform only work for lambda and variable"

-- | take an expression and a continuation,
--   apply the continuation  cps converted version of the expression
transformT :: Expr -> Var -> LC CExpr
transformT (Lam l_c e3) k = _
transformT expr@(Var n) k = (ALam [k]) <$> transformM expr
transformT (App e2 e3) k = _

