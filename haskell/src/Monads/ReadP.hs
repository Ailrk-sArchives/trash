{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE MagicHash         #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes        #-}

module Monads.ReadP where

-- a little parser combinator in prelude.

import           GHC.Base           hiding (many)
import           GHC.List           (null, replicate)
import           GHC.Unicode        (isSpace)

import           Control.Monad.Fail

-- infixr 5 +++, <++


-- |  parser for type a.
-- return a list of possible parsed value and the rest
-- of the stream.
-- This is a super inefficient implementation...
type ReadS a = String -> [(a, String)]

-- Representing parsed result.
-- There are multiple possible results, so P is splited into
-- different cases to singal current parsing state.
data P a = Get (Char -> P a)
         | Look (String -> P a)
         | Fail
         | Result a (P a)
         | Final (NonEmpty (a, String))
         deriving Functor

-- operators of P
final :: [(a, String)] -> P a
final []     = Fail
final (r:rs) = Final (r:|rs)

-- get a parser back from parser result
run :: P a -> ReadS a
run (Get f) (c:s)     = run (f c) s
run (Look f) s        = run (f s) s
run (Result x p) s    = (x, s) : run p s
run (Final (r:|rs)) _ = (r:rs)
run _ _               = []

instance Applicative P where
  pure x = Result x Fail
  (<*>) = ap

instance Alternative P where
  empty = Fail
  Get f1 <|> Get f2        = Get (\c -> f1 c <|> f2 c)
  Result x p <|> q         = Result x (p <|> q)
  p <|> Result x q         = Result x (p <|> q)
  Fail <|> p               = p
  p <|> Fail               = p

  Final r <|> Final t      = Final (r <> t)
  Final (r:|rs) <|> Look f = Look (\s -> Final (r:|(rs ++ run (f s) s)))

instance MonadPlus P

instance Monad P where
  (Get f) >>= k      = Get (\c -> f c >>= k)
  (Look f) >>= k     = Look (\s -> f s >>= k)
  Fail >>= _         = Fail
  (Result x p) >>= k = k x <|> (p >>= k)
