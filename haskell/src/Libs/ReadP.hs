{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveFunctor     #-}
{-# LANGUAGE OverloadedStrings #-}
module Libs.ReadP where

-------------------------------------------------------------------------------
-- inefficient ReadS implementation

import           Control.Applicative
import           Control.Monad       (ap)
import           Data.List.NonEmpty  as NonEmpty
import           GHC.Base            hiding (many)
import           GHC.Unicode         (isSpace)
import           Prelude             hiding (ReadP, ReadS)

-- infixr 5 +++, <++

type ReadS a = String -> [(a, String)]

-------------------------------------------------------------------------------
-- P is a datatype that encodes operations of a parser.
-- What can P do?
-- P can:
--  - consume char from the input stream
--  - lookup in the input stream
--  - return a result on success
--  - return fail on faliure
--  - return a final state of the parser and finish parsing.
-- We encode these notions into a data type.
-- It's easy to encode data. E.g To indicate the parser is failed we only need
-- an empty data constructor Fail; To indicate the parser is finished we need
-- Final constructor with the current state of everything.
--
-- But get and lookup are behaviors, how do we encode those into a datatype? The
-- only thing we can do is to embed functions into each data constructor.

data P a
  = Get (Char -> P a)
  | Look (String -> P a)
  | Fail
  | Result a (P a)
  | Final (NonEmpty (a, String))
  deriving Functor

instance MonadFail P where
  fail _ = Fail

instance Applicative P where
  pure x = Result x Fail
  (<*>) = ap

-- P is a monad
-- It's clear that you can have list as a monad, or maybe be a monad.
-- But what does it mean to have P to be a monad? It's a sum type with  5
-- branches...
--
-- Well, you can compose these data constructors with each other. The type just
-- indicates we are working with these 5 consturcors, it doesn't convey other
-- information. For each constructor, we can define it's special
-- bind implementation: how does it interact with other constructor.
--
-- k : a -> P a: is a polymorphic function over P, so it works on all possible
-- constructor of P. Our monad defines given a constructor of P, a monadic
-- function k, what to do next.
--
-- In this sense, k can be though as continuation, and The monad is an
-- interpreter defines a small language that is described in haskell algebraic
-- data type.
--
-- So essentially we defined a dsl for a parser, and use monad to conpose
-- elements in the dsl. The dsl is static.
instance Monad P where
  (Get f) >>= k           = Get $ \c -> f c >>= k
  (Look f) >>= k          = Look $ \s -> f s >>= k
  Fail >>= _              = Fail
  (Result x p) >>= k      = k x <|> (p >>= k)
  (Final (r :| rs)) >>= k = final [ys' | (x, s) <- (r:rs) , ys' <- run (k x) s]

-- Define what happens when we have two operations side by side.
-- another way to compose.
-- Alternative doesn't care what's inside the applicative. It just combines two
-- applicatives like monoid.
instance Alternative P where
  empty = Fail
  Get f1 <|> Get f2          = Get $ \c -> f1 c <|> f2 c  -- combine two Gets

  Result x p <|> q           = Result x (p <|> q)     -- results are delivered asap
  p <|> Result x q           = Result x (p <|> q)

  Fail <|> p                 = p
  p <|> Fail                 = p

  Final r <|> Final t        = Final (r <> t)
  Final (r :| rs) <|> Look f = Look $ \s -> Final (r :| (rs ++ run (f s) s))
  Final (r :| rs) <|> p = Look $ \s -> Final (r :| (rs ++ run p s))
  Look f <|> Final r = Look $ \s -> Final (case run (f s) s of
                                             [] -> r
                                             (x:xs) -> (x :| xs) <> r)

  Look f <|> Look g = Look $ \s -> f s <|> g s
  Look f <|> p = Look $ \s -> f s <|> p
  p <|> Look f = Look $ \s -> p <|> f s

-- Now our P is a Monad, Applicative, and Alternative. So how does it evaluates?
-- 1. the dsl program consists of sequence of monadic functions k :: (a -> P b).
-- Each time we get a new P a, we need it to k and get a P b, and so on.
-- (a -> P b) give us the opportunity ot work with element within the monad.
--
-- Note monadic function means it takes a values, do something, return a new
-- value with some effects. This is essentially what imperative langauges does,
-- in fact, monad is one of the best way to get denotional semantic of
-- imperative programs.
--
-- 2. when we have two P a, we can combine them to get a single P a. Different
-- from monoid, alternatives carries effect, so combine two P a means also
-- perform the effect .
--
-- e.g
--
-- let x = (P a)
--     >>= (\a -> ... (P b))
--     >>= (\b -> ... (P c))
-- let y = (P a')
--     >>= (\a -> ... (P b'))
--     >>= (\b -> ... (P c'))
-- let z = x <|> y
--
-- this is the basic flow of the interpreter.

-- converts list of results to Final
final :: [(a, String)] -> P a
final []     = Fail
final (r:rs) = Final (r :| rs)

-- run a (P a) parser.
run :: P a -> ReadS a
run (Get f) (c:cs) = run (f c) cs
run (Look f) s     = run (f s) s
run (Result x p) s = (x, s) : run p s     -- run result appends result
run (Final rs) _   = NonEmpty.toList rs
run _ _            = []

-------------------------------------------------------------------------------
-- ReadP is just a polymorphic wrapper on top of P.
-- think it as a little compiler compiles combinators to P.

newtype ReadP a = R (forall b. (a -> P b) -> P b)

instance Functor ReadP where
  fmap h (R f) = R $ \k -> f (k . h)

instance Applicative ReadP where
  pure x = R (\k -> k x)
  (<*>) = ap

instance Monad ReadP where
  R m >>= f = R $ \k -> m (\a -> let R m' = f a
                                  in m' k)

instance MonadFail ReadP where
  fail _ = R $ \_ -> Fail

instance Alternative ReadP where
  empty = pfail
  (<|>) = (+++)

-------------------------------------------------------------------------------
-- ReadP Operations

pfail :: ReadP a
pfail = R (\_ -> Fail)

(+++) :: ReadP a -> ReadP a -> ReadP a
R f1 +++ R f2 = R $ \k -> f1 k <|> f2 k
