module ConvexHull where

import Data.List

-- convex hull..
-- break problem in to smaller parts, then compose them back up.
-- Given n points in plane
-- S = {(x, y) | i = 1, 2, 3, ... n}
-- Assume no two have the samse X coordinate no two have the same y coord.

-- convex hull is the smallest polygon containing all points in ConvextHull(S).
-- TODO

type Point = (Int, Int)

-- sort points on x coordinates.
sortPoints :: [Point] -> [Point]
sortPoints = sortBy (\a b -> compare (fst a) (fst b))

merge :: [Point] -> [Point]
merge = undefined

convexHull :: [Point]
convexHull = undefined
