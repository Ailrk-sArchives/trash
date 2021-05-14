{-# LANGUAGE Arrows #-}

module Cat.ArrowPractice where

import           Control.Arrow
-- a b c : some computation with input b and c

{-@ First let's check the arrow properties @-}
-- TODO

{-@ For each arrow you can draw a digram
              y
    x  +--(f)----+
    ---+         + --->
       +--(g)----+
              g
@-}

-- proc create an arrow. -< apply is arrow application
addA :: Arrow a => a b Int -> a b Int -> a b Int
addA f g = proc x -> do
  y <- f -< x
  z <- g -< x
  returnA -< y + z

-- without arrow syntax
addA' :: Arrow a => a b Int -> a b Int -> a b Int
addA' f g = arr (\x -> (x, x)) >>>
  first f >>> arr (\(y, x) -> (x, y)) >>>
  first g >>> arr (\(z, y) -> y + z)

addA'' :: Arrow a => a b Int -> a b Int -> a b Int
addA'' f g = f &&& g >>> arr (uncurry (+))


{-@ Mini arrow tutorial from
    https://stackoverflow.com/questions/4191424/what-are-arrows-and-how-can-i-use-them
@-}

{-@ Circuit practice from haskell wiki @-}
