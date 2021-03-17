{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE LambdaCase           #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns         #-}

module MiniProject.Lens2048 where

-- TODO this is incomplete, still need to add logic to add cubes

-- dependencies:
--  1. linear, a library for doing linear algebra.
--  2. lens. the full lens library.

-- https://www.nmattia.com/posts/2016-08-19-lens-linear-2048.html

import           Control.Lens
import           Control.Monad
import           Control.Monad.State
import           Data.Default
import           Data.Maybe
import           Data.Monoid            (Sum (..), getSum)
import           Linear
import           System.Console.ANSI
import qualified Text.PrettyPrint.Boxes as Boxes
import           Text.Read              hiding (get)


type Board = M44 (Maybe (Sum Integer))
type Game = StateT Board IO ()


-- need type synonym instance and flexible context for this.

instance Default Board where
  def = V4 n n n n
    where n = V4 Nothing Nothing Nothing Nothing


main :: IO ()
main = evalStateT loop def
  where
    loop :: Game
    loop = forever $ do
      liftIO clearScreen
      get >>= liftIO . Boxes.printBox . mkBox
      liftIO getLine >>= \case
        "h"                               -> rows %= merge
        "j"                               -> locs %= merge
        "k"                               -> cols %= merge
        "l"                               -> wors %= merge
        (parseCell -> Just (lx, ly, val)) -> lx . ly .= Just (Sum val)

    parseCell (words -> [x, y, v]) =
      (,,) <$> parseLens x
           <*> parseLens y
           <*> readMaybe v
    parseCell _ = Nothing

    parseLens "x" = Just _x
    parseLens "y" = Just _y
    parseLens "z" = Just _z
    parseLens "w" = Just _w


-- --------------------------------------------------------------------------
-- Draw
-- --------------------------------------------------------------------------


class Box a where
  mkBox :: a -> Boxes.Box

instance Box Board where
  mkBox v = Boxes.vsep 1 Boxes.center1 $ v ^.. traverse <&> mkBox

instance Box (V4 (Maybe (Sum Integer))) where
  mkBox v = Boxes.hsep 2 Boxes.center1 $ v ^.. traverse <&> Boxes.text . f
    where f = maybe "X" (show . getSum)


-- --------------------------------------------------------------------------
-- Game
-- --------------------------------------------------------------------------


instance Reversing (V4 a) where
  reversing v = V4 (v^._w) (v^._z) (v^._y) (v^._x)

merge :: (Eq a, Monoid a) => [a] -> [a]
merge (x:x':xs) | x == x' = (x <> x') : merge xs
merge (x:xs) = x : merge xs
merge [] = []

rows, wors, cols, locs :: Traversal' (M44 (Maybe a)) [a]
rows = traverse . list
wors = traverse . reversed . list
cols = transposed . rows
locs = transposed . wors

transposed :: Iso' (M44 a) (M44 a)
transposed = iso transpose transpose

list :: Iso' (V4 (Maybe a)) [a]
list = iso toList fromList
  where
    toList v = reverse $ catMaybes $ foldl (flip (:)) [] v
    fromList (xs :: [a]) = V4 (xs^?ix 0) (xs^?ix 1) (xs^?ix 2) (xs^?ix 3)
