module Cases where

f1 x = if x + 1 == 1 then "AWESOME" else "wut"

f x =
    case x + 1 == 1 of  -- handle cases for all data constructors
      True -> "AWESOME"
      False -> "wut"

pal xs =
    let y = xs == reverse xs in
        case y of
          True -> "Yes"
          False -> "No"

greetingIfCool :: String -> IO ()
greetingIfCool coolness =
    case cool of
      True -> putStrLn "Cool"
      False -> putStrLn "not cool"
    where cool = coolness == "cool?"
