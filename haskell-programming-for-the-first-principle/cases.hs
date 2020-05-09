module case where

f1 = if x + 1 == 1 then "AWESOME" else "wut"

f x =
    case x + 1 == 1 of
      True -> "AWESOME"
      False -> "wut"
