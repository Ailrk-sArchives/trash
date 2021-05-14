module Libs.Misc where

-- data structures from the container.
import qualified Data.Array          as A
import qualified Data.HashMap.Lazy   as HM
import qualified Data.Map            as M
import qualified Data.Sequence       as SQ
import qualified Data.Set            as S
import qualified Data.Vector         as V
import qualified Data.Vector.Unboxed as UV

import           Criterion.Main

{-@ Map is implemented as a biary tree.
    has struct key and lazy value.
    The size of the subtree is also encoded at each node
@-}

type Size = Int
data MyMap k a = Bin {-# UNPACK #-} !Size !k a !(MyMap k a) !(MyMap k a)
               | Tip

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


