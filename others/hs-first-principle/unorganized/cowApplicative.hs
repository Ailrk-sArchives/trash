module CowApplicative where

data Cow =
    Cow {
          name :: String
        , age :: Int
        , weight :: Int
        } deriving (Eq, Show)

nonEmpty :: String -> Maybe String
nonEmpty "" = Nothing
nonEmpty s = Just s

nonNegative :: Int -> Maybe Int
nonNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

cowFromString :: String -> Int -> Int -> Maybe Cow
cowFromString name age weight =
    Cow <$> nonEmpty name
        <*> nonNegative age
        <*> nonNegative weight

-- instead of using the longger version of case af statement,
-- the applicative and functor combination will keep passing the
-- function into the same structure of its parameter, so explicit
-- checking is omitted.
