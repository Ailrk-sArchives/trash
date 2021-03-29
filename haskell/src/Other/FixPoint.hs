module Other.FixPoint where

fix f =
  let x = f x
   in x

fact = fix (\rec n -> if n == 0 then 1 else n * rec (n -1))

v1 = fact 5
