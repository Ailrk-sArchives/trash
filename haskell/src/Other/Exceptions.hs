{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs                     #-}

module Other.Exceptions where

-- exception mechanism in haskell
-- The exception monad transformer nicely simulate exception in a purely functional
-- style. Essentially that's an glorified system built based on Either monad transformer.
-- There are also realy exceptions that are rised by IO, and haskell has mechansim to cope
-- with that.

-- Exceptions naturally form a heirachy, thus subtyping. How do we express that in haskell?
-- We use eistential quantification.


-- exception mechanism in haskell
-- The exception monad transformer nicely simulate exception in a purely functional
-- style. Essentially that's an glorified system built based on Either monad transformer.
-- There are also realy exceptions that are rised by IO, and haskell has mechansim to cope
-- with that.

-- Exceptions naturally form a heirachy, thus subtyping. How do we express that in haskell?
-- We use eistential quantification.
import           Control.Exception (ArithException (..), AsyncException (..)
import           Data.Typeable

-- how are exceptions structured?

-- ways to look at it:
-- 1. sort of super type for all e implements Exception'
-- 2. a type that erase the actual type  it contains.
data SomeException' = forall e. Exception' e => SomeException' e SomeException'

-- use GADT to for existential type
-- no need of explicit forall. It's saying there exists a Exception' a hide behind
-- the SomeExceptionGADT constructor.
--
-- The scope is limited to this constructor only.
-- (POLYMORPHIC PARASITE)
data SomeExceptionGADT where
  SomeExceptionGADT :: Exception' a => a -> SomeExceptionGADT

data BlockedIndefinitelyOnMVar' = BlockedIndefinitelyOnMVar' deriving Show
data BlockedIndefinitelyOnSTM' = BlockedIndefinitelyOnSTM' deriving Show

data ArithException'
  = Overflow'
  | Underflow'
  | LossOfPrecision'
  | DividedByZero'
  | Denormal'
  | RatioZeoDenominator'
  deriving Show

class (Typeable e, Show e) => Exception' e where
  toException' :: e -> SomeException'
  fromException' :: SomeException' -> Maybe e
  displayException' :: e -> String

instance Exception' BlockedIndefinitelyOnSTM' where
  toException' = undefined
  fromException' = undefined
  displayException' = undefined

instance Exception' BlockedIndefinitelyOnMVar' where
  toException' = undefined
  fromException' = undefined
  displayException' = undefined

instance Exception' ArithException' where
  toException' = undefined
  fromException' = undefined
  displayException' = undefined

{-@ Examples @-}

data MyException = forall e. (Show e, Typeable e) => MyException e

instance Show MyException where
  showsPrec p (MyException e) = showsPrec p e

multiError :: Int -> Either MyException Int
multiError n =
  case n of
    0 -> Left (MyException DivideByZero)    -- differet
    1 -> Left (MyException StackOverflow)   -- types
    _ -> Right n



-- TODO
