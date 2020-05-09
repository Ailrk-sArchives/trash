module LearnParser where

import Text.Trifecta

---------------------------------------------------------------
-- Parsers that are formatted differently.
-------------------------------------------------------------
type Token = Char
newtype Parser' a =
  Parser' { runParser' :: String -> Maybe (a, String) }

-- Hutton Mejei Parser.
newtype HMParser a = HMParser ([Token] -> [(a, [Token])])
type HMParser' a = String -> [(a, String)]

-- They are basically the samething.

-------------------------------------------------------------
-- some simple parser combinator demo.
-------------------------------------------------------------
stop :: Parser a
stop = unexpected "stop"

one :: Parser Char
one = char '1'

-- in (>>), the value a in monad m a get thrown away, but
-- the effect of the operation will remain and propagate
-- down the expression.
one' :: Parser a
one'= one >> stop

-------------------------------------------------------------
-- char parser vanela implement
-------------------------------------------------------------
char' :: Char -> Parser' Char
char' c =
  Parser' $ \s ->
    case s of
      (x:xs) -> if c == x then Just (c, xs) else Nothing
      _ -> Nothing


