{-# LANGUAGE RankNTypes #-}
module ScottEncoding where

-- Scott encoding.
-- You can use functions to encode algebraic data types.
-- So if you have function you don't really need
-- anything else.
-- With scott encoding you can even express algebraic data types
-- in c++.


import           Prelude hiding (concat, curry, foldl, foldr, fst, length, map,
                          null, snd, take, zip, (++))


newtype Mabe' a =
  Maybe' { runMaybe :: forall b. b -> (a -> b) -> b }

newtype List' a =
  List' { runList :: forall b. b -> (a -> List' a -> b) -> b }

newtype Either' a b =
  Either' { runEither :: forall c. (a -> c) -> (b -> c) -> c }

newtype Pair' a b =
  Pair' { runPair :: forall c. (a -> b -> c) -> c }
