module Moi where

newtype Moi s a =
  Moi { runMoi :: s -> (a, s) }

-- ((->) s -> (a, s))
instance Functor (Moi s) where
  fmap f (Moi g) = Moi $ \s -> let (a, b) = g s
                                in (f a, b)

instance Applicative (Moi s) where
  pure a = Moi $ (,) a
  Moi f <*> Moi g =
    Moi $ \s ->
      let (fa, s') = f s
          (a, s'') = g s'
       in (fa a, s'')

instance Monad (Moi s) where
  return = pure
  Moi f >>= g =
    Moi $ \s ->
      let (a, s') = f s
          ms = runMoi $ g a
       in ms s'

class Moiful s where
  mkMoi :: (s -> (a, s)) -> Moi s a
  putMoi :: s -> Moi s ()
  execMoi :: Moi s a -> s -> s
  evalMoi :: Moi s a -> s -> a
  modifyMoi :: (s -> s) -> Moi s ()


instance Moiful Integer where
  mkMoi = Moi
  putMoi s = mkMoi $ (,) ()
  execMoi sa s = snd $ runMoi sa s
  evalMoi sa s = fst $ runMoi sa s
  modifyMoi ss = mkMoi $ \s -> ((), ss s)
