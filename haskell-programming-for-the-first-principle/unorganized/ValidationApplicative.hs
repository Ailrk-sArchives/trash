module ValidationApplicative where

data Validation e a =
    Failure e
  | Success a
  deriving (Eq, Show)

data Errors =
    DividedByZero
  | StackOverFlow
  | MooglesChewedWires
  deriving (Eq, Show)

instance Functor (Validation e) where
    fmap f (Success a) = Success (f a)
    fmap f (Failure e) = Failure e

-- compare with either, the validation type combine errors with
-- different monoidal behavior.
instance Monoid e => Applicative (Validation e) where
    pure = Success
    (Success f) <*> (Success a) = Success (f a)
    (Failure e) <*> (Failure e') = Failure (e <> e')
    (Failure e) <*> (Success a) = Failure e
    (Success a) <*> (Failure e) = Failure e
