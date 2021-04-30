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

-- these two are not really the same... like at all.
-- what's similar is the structure. You can pass the value
-- to let the next function to handle.
-- or the continuation can only be called after the value / monad
-- is computed, thus sequenctial operation.
