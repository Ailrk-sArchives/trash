{-# LANGUAGE TemplateHaskell #-}
module Libs.LensPractice where

import           Control.Lens
import           Data.Char
import qualified Data.Map     as Map

t1 = ("hello", "world") ^. _2
t2 = set _2 42 ("hello", "world")
t3 = set (_2._1) 100 ("hello", ("world", "!!!"))
t4 = "hello" ^. to length   -- on liner lens

-- compose lenses
t5 =  ("hello", ("world", "!!!")) ^. _2._2.to length
t6 = view (_2._2.to length) ("hello", ("world", "!!!"))

-- .~ for set
t7 = _1 .~ "hello" $  ((), "world")
t8 = (_2._1) .~ 100 $ ("hello", ("world", "!!!"))
t9 = _2 .~ "hello" $ ("hello", "world")

t10 = Map.fromList [("hello", "there")] ^.at "hello"

-- this two are the same. & is for the reverse application.
t11 = Map.fromList [("hello", "there")] & at "hello" ?~ "world"
t12 = at "hello" ?~ "world" $ Map.fromList [("hello", "there")]

-- reverse application, works like |>. It's been there for the entire time just
-- nobody use it fro some reason.
t13 = [1..10]
    & fmap (+1)
    & foldr (+) 0


-- over works a bit like fmap
t14' = fmap succ [1, 2, 3]
t14 = mapped %~ succ $ [1, 2, 3]

t15' = fmap (\(a, b) -> (a, succ b)) [(1, 2), (1, 2), (2, 3)]
t15 = mapped._2 %~ succ $ [(1, 2), (1, 2), (2, 3)]

-- use template haskell to create dummy lenses.
data Bar a = Bar { _foo  :: [Int]
                 , _buzz :: Int
                 }
                 deriving Show
makeLenses ''Bar

data Foo a = Foo { _bar  :: Bar a
                 , _baz  :: Int
                 , _quux :: Int
                 }
                 deriving Show
makeLenses ''Foo

foo1 = Foo (Bar [12, 13, 14] 20) 10 10
t16 = foo1 ^. bar
t17 = foo1 ^. bar.buzz
t18 = foo1 ^. bar.foo.to length
t19 = foo1 ^. bar.foo.to sum
t20 = foo1 ^. bar.foo.to(length . map (+1))

t21 = foo1 & baz .~ 20

-- complex modification get composed.
t22 = foo1
    & bar.foo.traversed.index 2 .~ 2
    & bar.foo.traversed.index 1 .~ 10
    & quux .~ 8
    & baz .~ 99
    & (^. (bar.foo))
    & traversed.index 1 .~ 88


t23 = [1, 2, 3] & mapped %~ succ

t24 = ["abc", "def", "ghi"] & ix 1 . ix 2 %~ toUpper

t25 = "abc" & mapped .~ 'x'

t26 = Map.empty & at 3 ?~ (Just 10)

t27 = Map.empty ^? at 3

t28 = (('a', 'b', "asd"), "xyz") ^. _1 . _3

t29 = ('a', 'b') ^. _1 . (to (:"c"))

