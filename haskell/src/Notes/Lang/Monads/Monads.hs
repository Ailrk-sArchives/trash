module Notes.Lang.Monads.Monads where


-- source
-- https://wiki.haskell.org/All_About_Monads

-- Example 1
data Sheep = Sheep
  { name :: String
  , father :: Maybe Sheep
  , mother :: Maybe Sheep
  }
  deriving (Eq, Show)

ggfather = Sheep "ggf" Nothing Nothing
grandfather = Sheep "gf" (Just ggfather) Nothing
mom = Sheep "mom" (Just grandfather) Nothing
kid = Sheep "kid" Nothing (Just mom)

-- without monad bind
mothersPaternalGrandFather' :: Sheep -> Maybe Sheep
mothersPaternalGrandFather' s =
  case (mother s) of
    Nothing -> Nothing
    Just m -> case (father m) of
                Nothing -> Nothing
                Just m -> Just m

-- with monad bind
mothersPaternalGrandFather :: Sheep -> Maybe Sheep
mothersPaternalGrandFather s = (Just s) >>= mother >>= father

-- do notation as syntatic sugar
mothersPaternalGrandFatherdo :: Sheep -> Maybe Sheep
mothersPaternalGrandFatherdo s = do
  m <- mother s
  gf <- father m
  father gf

-- equivalence with bind
mothersPaternalGGrandFather :: Sheep -> Maybe Sheep
mothersPaternalGGrandFather s =
  mother s >>= (\m ->
    father m >>= (\gf ->
      father gf))

-- list monad
dupThemselves ::  Int -> [a] -> [a]
dupThemselves n = (replicate n =<<)

-- monad law

