module Iso where

-- function contained in netype better to be isomorphic.
-- to the type it wraps.

type Iso a b = (a -> b, b -> a)

newtype Sum a = Sum {getSum :: a}

isoCheck ::  Iso a (Sum a)
isoCheck = (Sum, getSum)
