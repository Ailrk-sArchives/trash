module Parse where

import Control.Applicative hiding (some, many)
import Control.Monad
import Data.Char

newtype Parser a = Parser {parse :: String -> [(a, String)]}

runParser :: Parser a -> String -> a
runParser m s =
  case parse m s of
    [(res, [])] -> res
    [(_, rs)] -> error "Not consume the entire stream"
    _ -> error "Parse failed"

item :: Parser Char
item = Parser $ \s ->
  case s of
    [] -> []
    (c : cs) -> [(c, cs)]

bind :: Parser a -> (a -> Parser b) -> Parser b
bind p f =
  Parser $ \s ->
    concatMap (\(a, s') -> parse (f a) s') $
      parse p s

unit :: a -> Parser a
unit a = Parser $
  \s -> [(a, s)]

instance Functor Parser where
  fmap f (Parser cs) =
    Parser $
      \s -> [(f a, b) | (a, b) <- cs s]

instance Applicative Parser where
  pure = return
  (Parser cs1) <*> (Parser cs2) =
    Parser $ \s ->
      [ (f a, s'')
        | (f, s') <- cs1 s,
          (a, s'') <- cs2 s'
      ]

instance Monad Parser where
  return = unit
  (>>=) = bind

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = option

combine :: Parser a -> Parser a -> Parser a
combine p q = Parser $ \s -> parse p s ++ parse q s

failure :: Parser a
failure = Parser (\cs -> [])

option :: Parser a -> Parser a -> Parser a
option p q = Parser $ \s ->
  case parse p s of
    [] -> parse q s
    res -> res

some :: (Alternative f) => f a -> f [a]
some v = some_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

many :: (Alternative f) => f a -> f [a]
many v = many_v
  where
    many_v = some_v <|> pure []
    some_v = (:) <$> v <*> many_v

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = item >>= \c -> if p c then pure c else failure

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

chainl :: Parser a -> Parser (a -> a -> a) -> a -> Parser a
chainl p op a = (p `chainl1` op) <|> return a

chainl1 :: Parser a -> Parser (a -> a -> a) -> Parser a
p `chainl1` op = p >>= \a -> rest a
  where
    rest a =
      ( do
          f <- op
          b <- p
          rest (f a b)
      )
        <|> return a

char :: Char -> Parser Char
char c = satisfy (c ==)

natural :: Parser Integer
natural = read <$> some (satisfy isDigit)

string :: String -> Parser String
string [] = pure []
string (c : cs) = char c >> string cs >> pure (c : cs)

token :: Parser a -> Parser a
token p = do
  a <- p
  spaces
  pure a

reserved :: String -> Parser String
reserved = token . string

spaces :: Parser String
spaces = many $ oneOf " \n\r"

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do
  s <- string "-" <|> pure []
  cs <- some digit
  pure $ read (s ++ cs)

parens :: Parser a -> Parser a
parens m = do
  reserved "("
  n <- m
  reserved ")"
  pure n
