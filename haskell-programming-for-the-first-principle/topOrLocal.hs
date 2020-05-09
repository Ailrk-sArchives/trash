module TopOrLocal where

topLevelFunc :: Integer -> Integer
topLevelFunc x = x + woot + topLevelVal
    where woot :: Integer
          woot = 10

topLevelFuncLet :: Integer -> Integer
topLevelFuncLet x = let woot :: Integer
                        woot = 10
                        moot :: Integer
                        moot = 100
                  in x + woot + moot + topLevelVal

topLevelVal :: Integer
topLevelVal = 5



