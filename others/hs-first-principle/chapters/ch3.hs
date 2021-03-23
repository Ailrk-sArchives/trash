module Ch3 where


addExclimationMark :: String -> String
addExclimationMark n = n ++ "!"

get4thChar :: String -> Char
get4thChar n = n !! 4

takeLastWord :: String -> String
takeLastWord n = drop 9 (addExclimationMark n)

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = str !! x
                where str = "Curry is awesome!"






