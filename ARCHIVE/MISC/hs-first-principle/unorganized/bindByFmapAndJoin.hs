module BindByFmapAndJoin where

import Control.Monad (join)

-- a >>= implemented with join and fmap along.
-- fmap apply a function with type (a -> m b) to m a
-- so it will become m (m a)
-- join :: m (m a) -> m a
fbind :: Monad m => (a -> m b) -> m a -> m b
fbind f ma = join $ fmap f ma


-- Monad can be regarded as fmap a function to a monadic
-- structure, then merge the outmost layer.

