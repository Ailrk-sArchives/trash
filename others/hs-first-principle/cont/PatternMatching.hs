module PatternMatching where

type BoolCPS r = r -> r -> r


true :: BoolCPS r
true x _ = x

false :: BoolCPS r
false _ x = x

check :: BoolCPS String -> String
check b = b "It's true" "It's false"
