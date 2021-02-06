module MapCar
  ( mapcar
  , maplist
  , mapcar
  ) where

import           Control.Monad
import           Data.IORef
import           System.IO.Unsafe

transpose []       = repeat []
transpose (xs:xss) = zipWith (:) xs (transpose xss)

-- | mapcar, can be map or zip
-- >>> 10
mapcar :: ([a] -> b) -> [[a]] -> [b]
mapcar f = fmap f . transpose

-- | maplist scans on sublist
maplist :: ([[a]] -> b) -> [[a]] -> [b]
maplist f = fmap f . transpose . (stairs <$>)
  where
    stairs []       = []
    stairs [x]      = [[x]]
    stairs t@(_:xs) = t : stairs xs

-- | cons and update
nconc :: [IORef [a]] -> IO [a]
nconc = join . fmap readIORef . foldl nconc' (newIORef []) . fmap pure

nconc' :: IO (IORef [a]) -> IO (IORef [a]) -> IO (IORef [a])
nconc' xio yio = do
  xref <- xio
  ys <-join $ readIORef <$> yio
  modifyIORef xref (<> ys)
  return xref

unsafeNonc :: [IORef [a]] -> [a]
unsafeNonc = unsafePerformIO . nconc

unsafeNconc' :: IORef [a] -> IORef [a] -> IORef [a]
unsafeNconc' xref yref = unsafePerformIO $ do
  ys <- readIORef yref
  modifyIORef xref (<> ys)
  return xref

-- update on first argument
mapcan :: ([a] -> b) -> [[IORef a]] -> IO [b]
mapcan = undefined

