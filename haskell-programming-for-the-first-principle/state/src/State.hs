module State where

-- State :: (s -> (a, s)) -> State s a
-- runState :: State s a -> s -> (a, s)
-- random :: (Random a, RandomGen g) =>
--           g -> (a, g)
-- Take a state as input, generate a tuple
-- of value and next state as output.
newtype State' s a =
  State' { runState' :: s -> (a, s) }

-- Isomorphic demo
-- Functions contained in the new type must
-- be isomorhpic to the type it wraps.
type Iso a b = (a -> b, b -> a)

newtype Sum a = Sum { getSum :: a }

sumiIsIso :: Iso a (Sum a)
sumiIsIso =  (Sum, getSum)

stateIsIso :: Iso (s -> (a, s)) (State' s a)
stateIsIso = (State', runState')


