module PL.Combinators where


-- Y combinator --
-- y =  \f -> (\x -> f (x x)) (\x -> f (x x))
-- this can not be used becasue it creates infinite type.

newtype Mu a = Roll { unRoll :: Mu a -> a }

fix :: (a -> a) -> a
fix = g <*> (Roll . g)
  where
    g = (. (>>= id) unRoll)


fibs :: () -> [Integer]
fibs () = 0 : 1 : fix fibs' 0 1
  where
    fibs' fnc f s =
      case f + s of
        n -> n `seq` n : fnc s n
