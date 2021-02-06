module ConditionExpr where

greetingIfCool :: String -> IO ()
greetingIfCool coolness =
    if cool coolness
       then putStrLn "cool!"
    else
       putStrLn "not cool"
    where cool v = v == "Hehe"

