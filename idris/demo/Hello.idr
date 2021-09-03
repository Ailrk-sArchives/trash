module Hello

-- note it's a dependent type language, values and types share a same namespace.

-- normal data delcaration
data N = Z | S N

-- GADT syntax
data Ls : Type -> Type where
  Nil : Ls a
  Cons : a -> Ls a -> Ls a

-- finally no ::
x : Int
x = 42

food : String
food = "Sausage machine"

bar : Char
bar = 'z'

quux : Bool
quux = False

-- idris functions must have declarations.
plus : N -> N -> N
plus Z  n = n
plus (S m) n = S (plus m n)

mult : N -> N -> N
mult Z n = Z
mult (S m) n = plus n (mult m n)

hello : IO ()
hello = putStrLn "Hello World"

-- functions in where clause also need type signature.
reverse : List a -> List a
reverse xs = revAcc [] xs where
  revAcc : List a -> List a -> List a
  revAcc acc [] = acc
  revAcc acc (x :: xs) = revAcc (x :: acc) xs

-- finally you can declare local data types.
foo : Int -> Int
foo x = case isLT of
            MyYes => x*2
            MyNo => x*4
    where
       data MyLT = MyYes | MyNo
       isLT : MyLT
       isLT = if x < 20 then MyYes else MyNo

even : Nat -> Bool
even Z = True
even (S k) = odd k where
  odd : Nat -> Bool
  odd Z = False
  odd (S k) = even k

main : IO ()
main = do
  putStrLn (show x)
  putStrLn ("from foo: " ++ show (foo 10))
  hello
