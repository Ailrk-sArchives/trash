module Clean where

import           System.Directory
import           Control.Monad
import Data.Foldable

main :: IO ()
main =  getCurrentDirectory >>= getDirectoryContents >>= \xs -> do

  let xs' = do
      x <- xs
      case x of
        "Clean.hs" -> mempty
        "." -> mempty
        ".." -> mempty
        ".exrc" -> mempty
        other -> case dropWhile (/= '.') other of
                   ".alg" -> mempty
                   ".md" -> mempty
                   _ -> return x

  for_ xs' (\x -> removeFile x)
