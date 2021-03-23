module Print3 where

myGreeting :: [Char]
myGreeting = "hello" ++ " world!"

hello :: String
hello = "hello"

world :: String
world = "world!"

main :: IO()
main = do
    putStrLn myGreeting
    putStrLn secondGreegting
             where secondGreegting = concat [hello, " ", world]



