{-# LANGUAGE GADTs #-}

module Other.ProbabilityMonad where

import Control.Applicative
import Control.Monad (ap, liftM)
import Data.List
import System.Random hiding (uniform)

type Probability = Double

-- Probability Monad
class Monad m => ProbabilityMonad m where
  choose :: Probability -> m a -> m a -> m a

class ProbabilityMonad m => SupportMonad m where
  support :: m a -> [a]

class ProbabilityMonad m => ExpMonad m where
  expectation :: (a -> Double) -> m a -> Double

class ProbabilityMonad m => SamplingMonad m where
  sample :: RandomGen g => m a -> g -> (a, g)

-- Prob Monad type
newtype PExp a = PExp ((a -> Double) -> Double)

instance Functor PExp where
  fmap = liftM

instance Applicative PExp where
  pure = return
  (<*>) = ap

instance Monad PExp where
  return x = PExp $ \h -> h x
  (PExp d) >>= k =
    PExp $ \h ->
      let apply (PExp f) = f
          g x = apply (k x) h
       in d g

instance ProbabilityMonad PExp where
  choose p (PExp d1) (PExp d2) =
    PExp $ \h -> p * d1 h + (1 - p) * d2 h

instance SupportMonad PExp where
  support (PExp h) = undefined

instance ExpMonad PExp where
  expectation h (PExp d) = d h

instance SamplingMonad PExp where
  sample = undefined

-- generic probability monad
data P a where
  R :: a -> P a
  B :: P a -> (a -> P b) -> P b
  C :: Probability -> P a -> P a -> P a

instance Functor P where
  fmap = liftM

instance Applicative P where
  pure = return
  (<*>) = ap

instance Monad P where
  return = R
  d >>= k = B d k

instance ProbabilityMonad P where
  choose = C

instance SupportMonad P where
  support (R x) = [x]
  support (B d k) = concat [support (k x) | x <- support d]
  support (C p d1 d2) = support d1 ++ support d2

instance ExpMonad P where
  expectation h (R x) = h x
  expectation h (B d k) = expectation g d
    where
      g x = expectation h (k x)
  expectation h (C p d1 d2) =
    (p * expectation h d1)
      + ((1 - p) * expectation h d2)

instance SamplingMonad P where
  sample (R x) g = (x, g)
  sample (B d k) g =
    let (x, g') = sample d g
     in sample (k x) g'
  sample (C p d1 d2) g =
    let (x, g') = random g
     in sample (if x < p then d1 else d2) g'

-- helper functions
prob :: Bool -> Probability
prob b = if b then 1 else 0

uniform :: [a] -> P a
uniform [x] = return x
uniform ls@(x : xs) =
  let p = 1.0 / fromIntegral (length ls)
   in choose p (return x) (uniform xs)
