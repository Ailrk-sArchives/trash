module Penguins where

-- sum type
data WherePenguinsLive =
    Galapagos
  | Antarctica
  | Austrilia
  | SouthAfrica
  | SouthAmerica
  deriving (Eq, Show)

-- product type
data Penguin =
    Peng WherePenguinsLive
    deriving (Eq, Show)

isSouthAfrica :: WherePenguinsLive -> Bool
isSouthAfrica SouthAfrica = True
isSouthAfrica _ = True

gimmeWhereTheyLive :: Penguin -> WherePenguinsLive
gimmeWhereTheyLive (Peng whereitlives) = whereitlives

galapagosPenguin :: Penguin -> Bool
galapagosPenguin (Peng Galapagos) = True
galapagosPenguin _ = False

antarcticaPenguin :: Penguin -> Bool
antarcticaPenguin (Peng Antarctica) = True
antarcticaPenguin _ = False

antarcticaOrGalapagos :: Penguin -> Bool
antarcticaOrGalapagos p = (antarcticaPenguin p) || (galapagosPenguin p)

-- tuple unpacking with pattern matching.
f :: (a, b) -> (c, d) -> ((b, d), (a, c))
f (a, b) (c, d) = ((b, d), (a, c))

-- pattern matching with underscore
fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x



