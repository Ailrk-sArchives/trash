{-# LANGUAGE DeriveGeneric #-}
module Libs.Misc where

-- data structures from the container.
import           Control.Monad
import qualified Data.Array                  as A
import qualified Data.HashMap.Lazy           as HM
import qualified Data.Map                    as M
import qualified Data.Sequence               as SQ
import qualified Data.Set                    as S
import           Data.Vector                 ((//))
import qualified Data.Vector                 as V

import           Control.Monad.ST
import qualified Data.Vector.Unboxed         as UV

import qualified Data.Vector.Generic.Mutable as GM
import qualified Data.Vector.Mutable         as MV

import qualified Data.ByteString             as BS

import qualified Data.Text                   as T
import qualified Data.Text.Lazy              as TL

import qualified Data.Text.IO                as TIO
import qualified Data.Text.Lazy.IO           as TLIO

import qualified System.IO                   as SIO

import qualified Data.Text.Encoding          as TE
import qualified Data.ByteString.Lazy as BL


import           Criterion.Main


import           Data.Typeable
import           GHC.Generics

{-@ Map is implemented as a biary tree.
    has struct key and lazy value.
    The size of the subtree is also encoded at each node
@-}

type Size = Int
data MyMap k a = MapBin {-# UNPACK #-} !Size !k a !(MyMap k a) !(MyMap k a)
               | MapTip

-- benchmarking lookup on list
-- time                 147.6 μs   (146.9 μs .. 148.5 μs)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 147.8 μs   (147.4 μs .. 148.6 μs)
-- std dev              2.044 μs   (1.264 μs .. 3.381 μs)

-- benchmarking lookup on Map
-- time                 264.8 ns   (264.6 ns .. 265.2 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 265.1 ns   (264.9 ns .. 265.9 ns)
-- std dev              1.325 ns   (469.4 ps .. 2.488 ns)

mapBench :: IO ()
mapBench = defaultMain [ bench "lookup on list" $ whnf (lookup "doesntExist") pairList
                       , bench "lookup on Map" $ whnf (M.lookup "doesntExist") testMap
                       ]
  where

    genList :: Int -> [(String, Int)]
    genList n = go n []
      where
        go 0 xs  = ("0", 0) : xs
        go n' xs = go (n' - 1) ((show n', n') : xs)

    pairList :: [(String, Int)]
    pairList = genList 9001

    testMap :: M.Map String Int
    testMap = M.fromList pairList


-- Set

-- benchmarking member with Map
-- time                 246.4 ns   (245.7 ns .. 247.2 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 246.8 ns   (246.2 ns .. 248.9 ns)
-- std dev              3.678 ns   (1.396 ns .. 7.302 ns)
-- variance introduced by outliers: 16% (moderately inflated)

-- benchmarking member with Set
-- time                 244.4 ns   (242.8 ns .. 246.3 ns)
--                      1.000 R²   (0.999 R² .. 1.000 R²)
-- mean                 243.7 ns   (242.7 ns .. 247.7 ns)
-- std dev              5.620 ns   (1.948 ns .. 12.17 ns)
-- variance introduced by outliers: 32% (moderately inflated)


data MySet a
  = SetBin {-# UNPACK #-} !Size !a !(MySet a) !(MySet a)
  | SetTip


bumpIt (i, v) =  (i + 1, v + 1)

m :: M.Map Int Int
m = M.fromList $ take 10000 stream
  where stream = iterate bumpIt (0, 0)

s :: S.Set Int
s = S.fromList $ take 10000 stream
  where stream = iterate (+1) 0

memberMap :: Int -> Bool
memberMap i = M.member i m

memberSet :: Int -> Bool
memberSet i = S.member i s

setBench = defaultMain [ bench "member with Map" $ whnf memberMap 9999
                       , bench "member with Set" $ whnf memberSet 9999
                       ]


-- Sequence (finger tree.)
-- good for cons and append to both end, concatenation

newtype Seq a = Seq (FingerTree (Elem a))

newtype Elem a = Elem { getElem :: a }

data Digit a
  = One a
  | Two a a
  | Three a a a
  | Four a a a a
  deriving (Show, Generic)

data Node a = Node2 !Int a a | Node3 !Int a a a

data FingerTree a
  = Empty
  | Single a
  | Deep {-# UNPACK #-} !Int !(Digit a)  (FingerTree (Node a)) !(Digit a)

lists :: [[Int]]
lists = replicate 10 [1..100000]

seqs :: [SQ.Seq Int]
seqs = replicate 10 (SQ.fromList [1..100000])

-- benchmarking lists:
-- time                 9.481 ms   (9.414 ms .. 9.529 ms)
--                      0.999 R²   (0.999 R² .. 1.000 R²)
-- mean                 9.750 ms   (9.686 ms .. 9.866 ms)
-- std dev              234.9 μs   (155.4 μs .. 372.7 μs)

-- benchmarking seq:
-- time                 8.809 ms   (8.791 ms .. 8.824 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 8.818 ms   (8.809 ms .. 8.832 ms)
-- std dev              30.38 μs   (20.65 μs .. 51.14 μs)


seqBench1 = defaultMain [ bench "lists:" $ nf mconcat lists
                        , bench "seq: " $ nf mconcat seqs
                        ]


-- benchmarking lists:
-- time                 22.05 μs   (22.04 μs .. 22.07 μs)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 22.06 μs   (22.05 μs .. 22.08 μs)
-- std dev              53.36 ns   (30.97 ns .. 91.95 ns)

-- benchmarking seq:
-- time                 100.2 ns   (100.1 ns .. 100.2 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 100.2 ns   (100.2 ns .. 100.3 ns)
-- std dev              180.7 ps   (150.0 ps .. 210.6 ps)

-- accessing is much faster.
seqBench2 = defaultMain [ bench "lists:" $ whnf (\xs -> xs !! 9001)  [1..100000]
                        , bench "seq: " $ whnf (flip SQ.index 9001) (SQ.fromList [1..100000])
                        ]

-- note: sequence has worse memory locality.

data MyVec a = Vector {-# UNPACK #-} !Int
                      {-# UNPACK #-} !Int
                      {-# UNPACK #-} !(A.Array Int a)
                      deriving (Typeable)

-- vector supports efficient slicing.

listSlice :: Int -> Int -> [a] -> [a]
listSlice from len xs = take len (drop from xs)


-- benchmarking list slicing
-- time                 415.4 ns   (415.0 ns .. 415.8 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 415.0 ns   (414.4 ns .. 415.6 ns)
-- std dev              1.931 ns   (1.565 ns .. 2.362 ns)

-- benchmarking vector slicing
-- time                 57.98 ns   (57.90 ns .. 58.12 ns)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 58.26 ns   (58.13 ns .. 58.40 ns)
-- std dev              430.1 ps   (343.9 ps .. 638.6 ps)

vecBench1 = defaultMain [ bench "list slicing" $ whnf (head . listSlice 100 900) [1..1000]
                        , bench "vector slicing" $ whnf (V.head . V.slice 100 900) (V.fromList [1..1000])
                        ]


-- updating persistent vectors with fusion


testV' :: Int -> V.Vector Int
testV' n = fmap (+n) . fmap (+n) . fmap (+n) . fmap (+n) $ V.fromList [1..100000]

testV :: Int -> V.Vector Int
testV n = fmap ((+n) . (+n) . (+n) . (+n)) $ V.fromList [1..100000]

vecBench2 = defaultMain [ bench "vector composed: " $ whnf testV' 9998
                        , bench "vector fused by lib: " $ whnf testV 9998
                        ]

-- bactch update of vector.

-- benchmarking slow
-- time                 41.16 ms   (34.98 ms .. 45.55 ms)
--                      0.906 R²   (0.741 R² .. 0.977 R²)
-- mean                 39.41 ms   (35.41 ms .. 45.53 ms)
-- std dev              10.15 ms   (6.738 ms .. 15.71 ms)
-- variance introduced by outliers 79% (severely inflated)

-- benchmarking batch update:
-- time                 6.480 ms   (6.441 ms .. 6.528 ms)
--                      0.999 R²   (0.998 R² .. 1.000 R²)
-- mean                 6.307 ms   (6.254 ms .. 6.357 ms)
-- std dev              150.8 μs   (121.9 μs .. 194.4 μs)

-- update one by one
slow :: Int -> V.Vector Int
slow n = go n (V.fromList [1..100000])
  where go 0 v = v
        go n v = go (n - 1) (v // [(n, 0)])

batch :: Int -> V.Vector Int
batch n = (V.fromList [1..100000]) // updates
  where updates = fmap (\n -> (n, 0)) [0..n]

vecBench3 = defaultMain [ bench "slow: " $ whnf testV' 9998
                        , bench "batch update: " $ whnf testV 9998
                        ]


-- vector mutable update
mutableUpdateIO :: Int -> IO (MV.MVector RealWorld Int)
mutableUpdateIO n = do
  mvec <- GM.new (n + 1)
  go n mvec
  where
    go 0 v = return v
    go n v = (MV.write v n 0) >> go (n - 1) v


mutableUpdateST :: Int -> V.Vector Int
mutableUpdateST n = runST $ do
  mvec <- GM.new (n + 1)
  go n mvec
  where
    go 0 v = V.freeze v
    go n v = (MV.write v n 0) >> go (n - 1) v

-- note the STRep
-- type STRep s a = GHC.Prim.State# s -> (# GHC.Prim.State# s, a #)

-- benchmarking mutable vecotor through IO:
-- time                 5.371 ms   (5.362 ms .. 5.381 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 5.371 ms   (5.364 ms .. 5.383 ms)
-- std dev              27.65 μs   (17.44 μs .. 46.86 μs)

-- benchmarking mutable vecotor through ST:
-- time                 5.432 ms   (5.424 ms .. 5.442 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 5.439 ms   (5.436 ms .. 5.443 ms)
-- std dev              11.01 μs   (8.061 μs .. 16.66 μs)

-- persistent batch update is about 9x slower then mutable update.

vecBench4 = defaultMain [ bench "mutable vecotor through IO:" $ whnfIO (mutableUpdateIO 9998)
                        , bench "mutable vecotor through ST:" $ whnf mutableUpdateST 9998
                        ]

{-@ Strings @-}

-- Text has compact representation in memory and supports efficient indexing.


-- lazy. only lod on read.
dwords :: IO String
dwords = SIO.readFile "/usr/share/dict/cracklib-small"

-- load all content upfront.
dwordsT :: IO T.Text
dwordsT = TIO.readFile "/usr/share/dict/cracklib-small"

dwordsTL :: IO TL.Text
dwordsTL = TLIO.readFile "/usr/share/dict/cracklib-small"


readFileAndPrint :: IO ()
readFileAndPrint = do
  replicateM_  10 (dwords >>= print)
  replicateM_  10 (dwordsT >>= TIO.putStrLn)
  replicateM_  10 (dwordsTL >>= TLIO.putStrLn)
