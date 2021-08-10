{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}

module ClosureConversion () where

import           Data.Char                    (isDigit, isLetter, isSpace)
import qualified Data.HashMap.Strict          as HM
import           Text.ParserCombinators.ReadP as P

type Var = String

data Expr where
  Var :: Var -> Expr
  Apply :: Expr -> [Expr] -> Expr
  Lambda :: [Expr] -> Expr -> Expr
-- closure conversion only
  LambdaConverted :: [Expr] -> Expr -> Expr  -- converted lambda
  MkClosure :: Expr -> Expr -> Expr            -- clousure = lambda + env
  MkEnv :: Expr -> Expr -> Expr
  EnvRef :: Expr -> Expr -> Expr
  ApplyClosure :: Expr -> [Expr] -> Expr

deriving instance Show Expr

-------------------------------------------------------------------------------
-- parser
--

identifierp = idp <* skipSpaces
  where
    idp = do
      first <- satisfy isLetter
      rest <- munch (\c -> isDigit c || isLetter c)
      return $ first : rest

-- >>> P.readP_to_S reservedp "lambda"

tokenp :: Char -> ReadP Char
tokenp c = char c <* skipSpaces

reserved :: String -> ReadP String
reserved s = string s <* skipSpaces

varp :: P.ReadP Expr
varp = Var <$> identifierp

-- (f as)
applyp :: P.ReadP Expr
applyp = do
  tokenp ('(')
  Apply <$> parse <*> manyTill parse (tokenp ')')

lambbdaBodyp :: ([Expr] -> Expr -> Expr) -> ReadP a -> ReadP Expr
lambbdaBodyp con prefix = prefix *> (tokenp '(') *> do
  ts <- manyTill identifierp (tokenp ')')
  expr <- parse
  return $ con (fmap Var ts) expr

lambdap :: P.ReadP Expr
lambdap = lambbdaBodyp Lambda (reserved "lambda")

-- >>> P.readP_to_S parse "xc"

lambdaConvertedp :: P.ReadP Expr
lambdaConvertedp = lambbdaBodyp LambdaConverted (reserved "lambda*")

mkClosurep :: P.ReadP Expr
mkClosurep = MkClosure <$> (reserved "mkclosure" *> parse) <*> parse

mkEnvp :: P.ReadP Expr
mkEnvp = MkEnv <$> (reserved "mkenv" *> varp) <*> parse

envRefp:: P.ReadP Expr
envRefp = EnvRef <$> (reserved "envref" *> varp) <*> varp

applyClosurep :: P.ReadP Expr
applyClosurep = ApplyClosure
            <$> (reserved "apply-closure" *> parse)
            <*> (manyTill parse (tokenp ')'))

parse :: P.ReadP Expr
parse = lambdaConvertedp
    <++ lambdap
    <++ mkClosurep
    <++ mkEnvp
    <++ envRefp
    <++ varp
    <++ applyClosurep
    <++ between (tokenp '(') (tokenp ')') parse
    <++ applyp

substitue :: Expr -> Expr
substitue = undefined

closureConvert :: Expr -> Expr
closureConvert = undefined

transformBottomUp :: (Expr -> Expr) -> Expr -> Expr
transformBottomUp = undefined

transformTopdown :: (Expr -> Expr) -> Expr -> Expr
transformTopdown = undefined

flatClosureConvert :: Expr -> Expr
flatClosureConvert = transformBottomUp closureConvert

sharedClosureConvert :: Expr -> Expr
sharedClosureConvert = transformTopdown closureConvert
