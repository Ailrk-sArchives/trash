module Flip where


flips :: (a -> b -> c) -> b -> a -> c
flips f x y = f y x

returnLast :: a -> b -> c -> d -> d
returnLast _ _ _ d = d

data Employee = Coder
              | Manager
              | Veep
              | CEO
              deriving (Eq, Ord, Show)

reportBoss :: Employee -> Employee -> IO ()
reportBoss e e' =
    putStrLn $ show e ++ " is the boss of " ++ show e'

employeeRank :: (Employee -> Employee -> Ordering)
             -> Employee
             -> Employee
             -> IO ()

employeeRank f e e' =
    case f e e' of
      GT -> reportBoss e e'
      EQ -> putStrLn "Neither emploee is the boss"
      LT -> reportBoss e' e

codersRuleCEOsDrool :: Employee -> Employee -> Ordering
codersRuleCEOsDrool Coder Coder = EQ
codersRuleCEOsDrool Coder _ = GT
codersRuleCEOsDrool _ Coder = LT
codersRuleCEOsDrool e e' = compare e e'

-- guard syntax use | pipe sign to begin a guard case.
newAbs :: Integer -> Integer
newAbs x
    | x < 0 = -x
    | otherwise = x

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
    | a^2 + b^2 == c^2 = "Right angle"
    | otherwise = "not right"

-- function composition (f. g) x = f (g x)
takeFirstFiveOdd :: (Integral a, Enum a) => a -> [a]
takeFirstFiveOdd = take 5 . filter odd . enumFrom

-- same function written without function composition
takeFirstFiveOdd1 :: (Integral a, Enum a) => a -> [a]
takeFirstFiveOdd1 n = take 5 (filter odd (enumFrom n))

-- same function in point free style in a sense ...
takeFirstFiveOddPointFree :: (Integral a, Enum a) => a -> [a]
takeFirstFiveOddPointFree = \n -> take 5 . filter odd . enumFrom $ n


-- rewrite to point free the point is to remove argument
foldPlus :: Int -> [Int] -> Int
foldPlus = foldr (+)

numOfA :: String -> Int
numOfA = length . filter (== 'A')


