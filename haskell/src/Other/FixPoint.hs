{-# LANGUAGE RankNTypes #-}
module Other.FixPoint where

-- y combinator.

-- fix f = f (f (f ...))
fix f =
  let x = f x
   in x

fact = fix f
  where f rec n | n == 0 = 1 | otherwise = n * rec (n -1)

many_ones = fix (1:)
onst_still_const = fix (const "hello")

v1 = fact 5

-- it's not only useful for expressing arbitrary recursion in lambda calculus.
-- the usefulness extend to the type system, since now you can create a type
-- that encompass arbitrarily nested types.

-- this is just Y combinator, and in lambda calculus without let binding you do
-- recursion with it.

-- For typed lambda calculus, as long as fix is introduced, every type becomes
-- inhabited.
-- As fix (\x:T. x) as type T, but it denotationally has type bottom.
-- so once we have fix, we can no longer guarantee that every well typed term
-- reduce to a value.

