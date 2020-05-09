module DatabaseProcessing where

import Data.Time

data DatabaseItem = DbString String
                  | DbNumber Integer
                  | DbDate UTCTime
                  deriving (Eq, Ord, Show)

theDatabse :: [DatabaseItem]
theDatabse =
    [ DbDate (UTCTime
              (fromGregorian 1911 5 1)
              (secondsToDiffTime 34123))
    , DbDate (UTCTime
              (fromGregorian 1931 5 1)
              (secondsToDiffTime 34123))
    , DbDate (UTCTime
              (fromGregorian 1981 5 1)
              (secondsToDiffTime 34123))
    , DbNumber 9001
    , DbNumber 306
    , DbNumber 803
    , DbNumber 9003
    , DbNumber 904
    , DbNumber 1001
    , DbNumber 301
    , DbString "Hello, World!"
    , DbString "World!"
    , DbString "Hello!"
    , DbString "Jojo!"
    , DbDate (UTCTime
              (fromGregorian 1921 5 1)
              (secondsToDiffTime 34123))
    ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate =
    foldr consDate []
        where consDate a b =
                case a of
                  (DbDate date) -> date:b
                  _ -> b

-- handle all cases. the pattern matching works just like branches.
filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber [] = []
filterDbNumber (DbNumber x:[]) = [x]
filterDbNumber (_:[]) = []
filterDbNumber (DbNumber x:xs) = x:filterDbNumber xs
filterDbNumber (_:xs) = filterDbNumber xs

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = maximum . filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = sum . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x = fromIntegral
          (sum . filterDbNumber $ x)
          / (fromIntegral $ length $ filterDbNumber x)

