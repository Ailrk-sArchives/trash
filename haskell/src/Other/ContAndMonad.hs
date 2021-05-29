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

-- why continuation and monad are similar?
--
--   why monad and cps both keep sequetential order?
--   it's not because you can do it step by step, it's
--   simply because there they both have nested structures.
--
--   How do you keep evaluation in order in LC?
--   there is nothing you can do to directly express it, but what
--   you can do is nest on function in another.
