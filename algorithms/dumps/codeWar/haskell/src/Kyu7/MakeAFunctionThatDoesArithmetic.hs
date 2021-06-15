-- <Make a function that does arithmetic!>
module Kyu7.MakeAFunctionThatDoesArithmetic where

data Operation = Add | Divide | Multiply | Subtract deriving (Eq, Show, Enum, Bounded)

----------------------------------------------------
-- first attempt
----------------------------------------------------
arithmetic :: Fractional a => a -> a -> Operation -> a
arithmetic a b operator =
  case operator of
    Add -> a + b
    Divide -> a / b
    Multiply -> a * b
    _ -> a - b


----------------------------------------------------
-- better solutions
----------------------------------------------------
-- By deriving from enum type, fromEnum give order of
-- data in data constructor.
arithmetic' :: Fractional a => a -> a -> Operation -> a
arithmetic' a b operator = a `op` b
  where op = [(+), (/), (*), (-)] !! fromEnum operator

