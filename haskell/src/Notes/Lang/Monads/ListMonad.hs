module Notes.Lang.Monads.ListMonad where

-- source from:
-- https://en.wikibooks.org/wiki/Haskell/Understanding_monads/List

-- bunny invasion
-- n ^ g numbers of bunny in generation g and population n

bunnyInvasion :: Int -> Int -> [String] -> [String]
bunnyInvasion n g bunny = bunny >>= genBunnies g
 where
  generation = replicate n
  genBunnies :: Int -> String -> [String]
  genBunnies c b | c == 1    = generation b
                 | c < 1     = [b]
                 | otherwise = generation b >>= genBunnies (c - 1)

-- [1, 2, 3] => [1, 2, 2, 3, 3, 3]
themselveTimes :: [Int] -> [Int]
themselveTimes = (f =<<)
  where f n = [n | _ <- [1..n]]


-- list coprehensions
