{-# LANGUAGE NoMonomorphismRestriction #-}

module Ch5 where


example = 1

f1 :: Num a => a
f1 = (* 9) 6

f2 :: Num a => (a, String)
f2 = head [(0, "doge"), (1, "kitteh")]

f3 :: (Integer, String)
f3 = head [(0 :: Integer, "doge"), (1, "kitteh")]

f4 :: Bool
f4 = if False then True else False

f5 :: Int
f5 = length [1, 2, 3, 4, 5]

f6 :: Bool
f6 = (length [1, 2, 3, 4]) > (length "TACOCAT")

myFunc :: (x -> y)
       -> (y -> z)
       -> c
       -> (a, x)
       -> (a, z)
myFunc xToy yToz _ (a, x) = (a, (yToz (xToy x)))

-- e1
f :: Int -> String
f = undefined

g :: String -> Char
g = undefined

h :: Int -> Char
h i = g (f i)

-- e2
data A
data B
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e a = w (q a)

-- e3
data X
data Y
data Z

xz :: X -> Z
xz = undefined

yz :: Y -> Z
yz = undefined

xform :: (X, Y) -> (Z, Z)
xform (x, y) = (xz x, yz y)

-- e4
munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge xToy yTowz x = fst (yTowz (xToy x))

