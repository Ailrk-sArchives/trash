{-# LANGUAGE CPP #-}
module GHCs.Bench where

import           Criterion.Main

-- bench our own indexing operator


#ifdef NAIVE_VERSION
infixl 9 !?
_ !? n | n < 0 = Nothing
[] !? _ = Nothing
(x:_) !? 0 = Just x
(_:xs) !? n = xs !? (n - 1)
#endif

infixl !?

-- whnf measures how quick the code reaches the position.
-- nf measures the full evaluation time.

-- note (\x r k -> ...) as type a -> b -> c -> d -> a -> (b -> c) -> d
-- b -> c is our base case
(!?) :: Num a => [a] -> Int -> Maybe a
xs !? n
  | n < 0 = Nothing
  | otherwise = foldr (\x r k -> case k of
                                   0 -> Just x
                                   _ -> r (k - 1)) (const Nothing) xs n
{-# INLINABLE (!?) #-}
{-# SPECIALIZE (!?) :: [Integer] -> Int -> Maybe Integer #-}


xs :: [Integer]
xs = [1..9999]

-- when doing benchmarking in haskell, it's important to think about if you want to benchmark
-- the value get evaluated to weak head normal form or normal form.

main :: IO ()
main = defaultMain [ bench "index list 9999" $ whnf (xs !!) 9998
                   , bench "index list maybe index 9999" $ whnf (xs !?) 9998
                   ]

xs1 :: [Integer]
xs1 = [1..9999] ++ [undefined]

-- nf on xs1 will evaluate the undefined.
main1 :: IO ()
main1 = defaultMain [ bench "index list maybe index 9999" $ whnf (xs1 !?) 9999 -- ok
                    , bench "index list 9999" $ whnf (xs1 !!) 9999  -- crash
                    , bench "index list maybe index 9999" $ nf (xs1 !?) 9999  -- crash
                    ]

-- you need nf for map beacuse you also want to know the overhead of f.
main2:: IO ()
main2 = defaultMain [ -- 12.63ns all it evals to is _:_
                      bench "map to list whnf" $ whnf (map (+1)) xs
                    , bench "map to list nf" $ nf (map (+1)) xs]      -- 1.133 ms
