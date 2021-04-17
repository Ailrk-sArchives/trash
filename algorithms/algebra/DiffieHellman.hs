module DiffieHellman where

dhsend :: Integer -> Integer -> Integer -> (Integer -> Integer)
dhsend p g a = \b -> (g ^ a ^ b) `mod` p

-- we both have g, the primitive element being 5.

-- me send you
-- I pick a = 6
r = dhsend 23 5 6 1

-- you send me
-- you pick a = 9
s = dhsend 23 5 9 1

-- I receive yours as 11
k1 = (s ^ 6) `mod` 23


-- notice if I call dhsend 23 5 6 s it will hang.
-- but we can rely on the fact we are working on a ring and can
-- reduce large number to smaller one, ((g^a) mod p)^b mod p takes
-- much less computation.
