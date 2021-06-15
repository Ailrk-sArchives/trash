{-# LANGUAGE OverloadedStrings #-}
module Kyu1.SimpleInteractiveInterpreter where


import           Control.Monad.Trans.State
import qualified Data.HashMap.Strict       as HM
import           Text.Parsec



type Name = String


data Expr
  = Let Name Expr Expr
  | Var String
  | BinOp Name Expr Expr
  | UnaryOp Name Expr
  | Number Float
  | Fn Name [Name] Expr
  | Call Name [Expr]
  deriving (Show, Eq, Ord)

data Interpreter
type Result = Maybe Double


newInterpreter :: Interpreter
newInterpreter = undefined

input :: String -> Interpreter -> Either String (Result, Interpreter)
input _ _ = Left "Not implemented"
