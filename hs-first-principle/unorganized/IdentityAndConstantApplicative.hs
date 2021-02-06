module IdentityAndConstantApplicative where

-- Idenitity a
newtype Identity a = Identity a deriving (Eq, Show, Ord)

instance Functor Identity where
    fmap f (Identity a) = Identity $ (f a)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity (f a)


-- Constant a b

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    Constant a <*> Constant a' = Constant (a <> a')
