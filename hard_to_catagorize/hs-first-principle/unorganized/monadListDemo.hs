module MonadListDemo where

-- list monad
twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
       then [x * x, x * x]
       else []

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs =
    xs >>=
        \x -> if even x
                 then [x * x, x * x]
                 else []

-- Maybe Monad
data Cow = Cow {
    name :: String
  , age :: Int
  , weight :: Int
  } deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n
  | n >= 0 = Just n
  | otherwise = Nothing

weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
     in if n == "Bess" && w > 499 then Nothing else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name age weight = do
    nammy <- noEmpty name
    agey <- noNegative age
    weighty <- noNegative weight
    weightCheck (Cow nammy agey weighty)

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'  name age weight =
    noEmpty name >>=
        \namey ->
            noNegative age >>=
                \agey ->
                    noNegative weight >>=
                        \weighty ->
                            weightCheck (Cow namey agey weighty)



