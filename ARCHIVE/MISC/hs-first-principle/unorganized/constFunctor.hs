module ConstFunctor where

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant m) where
    fmap _ (Constant v) = Constant v

-- this will return (Constant 9) because the (const 2 will no be applied)
-- Constant 9 (const 2 b), b is the phahtom type without a value in term level.
constMapOnConstant = fmap (const 2) (Constant 9)
-- this will be Just 9 because Just (const 2 9)
constMapOnJust = fmap (const 2) (Just 9)


