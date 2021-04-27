module Monads.MTLDefaultSignature where

-- one can use default signature to reduce the amount of
-- boilerplates when adding a new mtl style typeclass.
--
-- Ideally we want it just automatically work, but monad
-- transformers can behave differently, because monad doens't
-- compose very well.
--
-- No matter what, we still need to specify an instance for
-- MTL typeclass with the monad (or monad transformer) it supoorts.

-- The key idea of MTL is the type parameter (Monad m) => m itself
-- can also be a monad, thus one can chain arbitray amount of
-- monads and adding new features at each layer.

-- How do we use MTL?
-- We stack much of monad transformers on top of each other as
-- a new type. This new type is now stand alone, contains all monads
-- in the stack, but support no operation what so ever.
--
-- Then we do a newtype deriving derive all the instance to the
-- top level. We derive MonadError, then the newtype will have
-- the instance MonadError, instead of the ExceptT part somewhere
-- in the transformer stack.
-- It's the same for other layers too, and eventually you get a giant
-- ass monad that has functionality of multiple smaller monads.

-- Notice to allow monad transformers stack on each others and still
-- bing able to deriving them from new type, we need to have the
-- corresponding instance between each two adjacent layers.
-- this is the core of n^2 instance problem.
