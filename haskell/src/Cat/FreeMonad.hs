module Cat.FreeMonad where


-- If we have a functor, we get a monad for free.

data Free f a = Pure a | Roll (f (Free f a))


-- operations maps between the functor and free monad.
liftFree :: Functor f => f a -> Free f a
liftFree = undefined

foldFree :: Functor f => (f r -> r) -> Free f r -> r
foldFree = undefined



