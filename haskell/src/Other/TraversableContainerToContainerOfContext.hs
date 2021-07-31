module Other.TraversableContainerToContainerOfContext where
-- https://stackoverflow.com/questions/48954495/is-it-possible-to-get-all-contexts-of-a-traversable-lazily

import Control.Monad.Trans.Cont

-- Fun with continuation
-- Turn any container into a container of context

-- a tuple of a functoin that takes paramter a and the paramter a.
-- a is the context.
newtype Hole a f = Hole { unHold :: (a, a -> f (Hole a f)) }

plug :: Hole a f -> f (Hole a f)
plug (Hole (a, f)) = f a

peek :: Hole a f -> a
peek (Hole (a, _)) = a

poke :: Functor f => Hole a f -> a -> f a
poke (Hole (_, f)) = fmap peek . f

-- replace each element with it's context in the given container
-- holes :: Traversable f => f a -> f (Hole a f)
-- holes xs = evalCont $ traverse (\a ->
--   callCC $ \k -> do
--     let kont :: a -> f (Hole a f)
--         kont a = evalCont (k (Hole (a, kont)))
--     pure $ Hole (a kont)) xs
