module ActivitySelecting where

import Control.Monad
import Data.List
import Debug.Trace

type Activity = (Int, Int)

sf :: [Activity]
sf =
  [ (3, 5),
    (3, 8),
    (0, 6),
    (5, 9),
    (1, 4),
    (8, 11),
    (8, 12),
    (5, 7),
    (12, 14),
    (6, 10),
    (2, 13)
  ]

-- select the tighest fit among activities.
-- fix the finishing point, the first task with larger starting point
-- is the task you want to schedule.


selectActivities sf = nub (mconcat [[a, b] | (a, b) <- select sf])
  where
    select :: [Activity] -> [Activity]
    select sf = trace (show sf1) $ go 0 0 []
      where
        sf1 = sortBy (\a b -> compare (snd a) (snd b)) sf
        s = fst . (sf1 !!)
        f = snd . (sf1 !!)
        go i j result
          | j == length sf1 =  result
          | s j >= f i = go j (j + 1) ((i, j) : result)
          | otherwise = go i (j + 1) result

run = selectActivities sf

selectActivitiesReverse sf = nub (mconcat [[a, b] | (a, b) <- select sf])
  where
    select :: [Activity] -> [Activity]
    select sf = trace (show sf1) $ go 0 0 []
      where
        sf1 = sortBy (\b a -> compare (snd a) (snd b)) sf
        s = fst . (sf1 !!)
        f = snd . (sf1 !!)
        go i j result
          | j == length sf1 = result
          | f j < s i = go j (j + 1) ((i, j) : result)
          | otherwise = go i (j + 1) result

run1 = selectActivitiesReverse sf
