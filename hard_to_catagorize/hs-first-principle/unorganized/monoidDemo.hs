module MonoidDemo where

data Optional a =
    Nada
  | Only a
  deriving (Eq, Show)

-- Note that there is a Monoid constrain for a.

-- Semigroup is a superclass for Monoid
instance Monoid a => Monoid (Optional a) where
    mempty = Nada

instance Semigroup a => Semigroup (Optional a) where
    Nada <> (Only a) = Only a
    (Only a) <> Nada = Only a
    (Only a) <> (Only a') = Only (a <> a')
    Nada <> Nada = Nada




