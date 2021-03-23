module AsPattern where
import Data.Char

isSubsequeneOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequeneOf [] _ = True
isSubsequeneOf (x:xs) par =
    elem x par && isSubsequeneOf xs par

captializeWords :: String -> Maybe [(String, String)]
captializeWords "" = Nothing
captializeWords s =
    let capHead (x:xs) = toUpper x : xs
        capHead [] = ""
        tokenize str = go str ""
            where
                go "" acc = [reverse acc]
                go (a:as) acc
                  | a == ' ' = reverse acc : go as ""
                  | otherwise = go as (a : acc)
        tuplize = map (\a -> (a, capHead a))
     in Just (tuplize . tokenize $ s)
