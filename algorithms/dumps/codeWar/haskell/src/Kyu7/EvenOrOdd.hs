-- <Even or Odd>
module Kyu7.EvenOrOdd where


-- for test suit set up
evenOrOdd :: Integral a => a -> String
evenOrOdd a = if mod a 2 == 0 then "Even" else "Odd"
