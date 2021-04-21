module EdmondsKarp where

-- capacity constraint.
-- skew symmetric.
-- flow conservation.

type Capacity = Integer

type Flow = Integer

type Graph = [(String, [(String, Integer)])]

graph =
  [ ("Vancouver", [("Edmonton", 16), ("Calgary", 13)]),
    ("Edmonton", [("Saskatoon", 12)]),
    ("Calgary", [("Edmonton", 4), ("Regina", 14)]),
    ("Saskatoon", [("Calgary", 9), ("Winnipeg", 20)]),
    ("Regina", [("Saskatton", 7), ("Winnipeg", 4)])
  ]


edmondsKarp :: [Graph] -> Integer
edmondsKarp = undefined
