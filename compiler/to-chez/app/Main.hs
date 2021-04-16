{-# LANGUAGE GADTs #-}

module Main where

import Data.Text as T
import Map as M
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String
import Text.Parsec.Token

type Name = String

data Lit
  = LitInt Int
  | LitStr T.Text
  deriving (Show, Eq)

data Expr
  = Var Name
  | Call Name [Name]
  | Lam [Name] Expr
  | Let [(Name, Expr)] Expr
  | Lit Lit
  deriving (Show, Eq)

litP :: Parser Lit
litP = intP <|> strP
  where
    intP = LitInt . read . (: "") <$> digit
    strP = LitStr . T.pack <$> (char '"' *> many anyChar <* char '"')

exprP :: Parser Expr
exprP = varp
  where
    identifier = do
      head <- (: []) <$> letter
      trailing <- many (letter <|> digit <|> char '_')
      pure (head ++ trailing)
    varp = Var <$> identifier
    callp = do
      name <- identifier
      args <- many identifier
      pure (Call name args)
    lamp = do
      params <- many identifier
      e <- exprP
      pure (Lam params e)
    letp = do
      string "let" *> spaces
      bindings <- many exprPairsp
      body <- exprP
      pure (Let bindings body)
      where
        exprPairsp = do
          name <- identifier
          e <- exprP
          pure (name, e)

main :: IO ()
main = putStrLn ""
