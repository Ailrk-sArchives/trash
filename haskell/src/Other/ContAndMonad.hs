module Other.ContAndMonad where

-- monad
foo :: Maybe Int
foo =
  Just 5 >>= \x ->
    Just 6 >>= \y ->
      return (x + y)

-- continuation
add :: Int -> Int -> (Int -> Int) -> Int
add x y k = k (x + y)

bar :: Int
bar =
  add 1 2 $ \x ->
    add x 4 $ \y ->
      y

-- meaningless comparision...

