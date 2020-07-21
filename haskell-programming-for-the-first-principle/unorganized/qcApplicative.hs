module QcApplicative where

import Test.QuickCheck

data Bull =
    Fools
  | Twoo
  deriving (Eq, Show)

instance Arbitrary Bull where
    aribitrary =
        frequency [ (1, return Fools)
                  , (1, return Twoo) ]

instance Monoid Bull where
    mempty = Fools
    (<>) _ _ = Fools

main :: IO ()
main = undefined




