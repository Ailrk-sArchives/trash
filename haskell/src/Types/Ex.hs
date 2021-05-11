{-# LANGUAGE DeriveAnyClass            #-}
{-# LANGUAGE DeriveGeneric             #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RankNTypes                #-}
module Types.Ex where

import           Control.Monad.ST
import           Data.IORef
import           Data.STRef
import           GHC.Generics

{-@ extential types
    first of all we have universal type ∀.
    ∀x.t means for all type x we have t. Here we don't have any assumption
    of x, so this type should work for every x.

    existential type is the opposite of universal type. We say
    ∃x.t, meaning exists a type x such that y.

    existential quantifier type can be encoded with universal quantifier:
    ∃x.t ≡ ∀y.(∀x.t → y) → y

@-}

-- We say there exists a value E such that E -> E.
data E = forall a. E a
ide :: E -> E
ide x = x


-- for all types s that's in Show, there exists SB s.t SB s :: *
data ShowBox = forall s. Show s => SB s

instance Show ShowBox where
  show (SB s) = show s

heterList :: [ShowBox]
heterList = [SB 1, SB True, SB "good"]

printShowboxes :: [ShowBox] -> IO ()
printShowboxes = mapM_ print

-- this is an any type
-- it just takes all types.
-- for all types a, exists All a
data AllBox = forall a. All a

-- forall can be thought of find the commonality of different types.
-- Note bottom is a member of all types!
-- So we are saying for the commonality of all types, we have All a
-- => The only commanlity of all types is they have bottom time as a member.
-- => All types are part of All a
anylist = [All 1, All "asd", All heterList]

-- why we need a box anyway?
-- see this
-- [forall a. a]  a list of forall a, the common types of all a, so... [_|_]
-- [forall a. Show a => a]  list of forall a that is part of Show, the common type of them. So [_|_]
-- [forall a. Num a => a] list of all a that's part of Num a, the common type of them. So [_|_]

-- but This is different:
data T = forall a. MkT a
-- type of MkT :: forall a. a -> T
-- a list allows any types, but we can't do anything to it because types
-- are forall a. a
tlist = [MkT (), MkT "asd", MkT 12]

data G = forall a. (Num a, Show a) => MkG a
-- type of MkG :: a. (Num a, Show a) => MkG a
-- Now we can do something based on the type constraint.
glist = [MkG 1, MkG 1.2]

{-@ The biggest application: RankNType with runST @-}

-- runST :: (forall s. ST s a) -> a
-- the state cannot escape the ST action, so runST constraint the
-- effect within the scope.
localMut :: Int -> Int
localMut n = runST $ do
  ref <- newSTRef n
  modifySTRef' ref (+1)
  readSTRef ref

-- define pair with universal quantification
newtype Pair a b = Pair (forall c. (a -> b -> c) -> c)

makePair :: a -> b -> Pair a b
makePair a b = Pair $ \f -> f a b

-- some other examples --
class Buffer a where
  flush :: a -> a
  read :: Int -> a
  write :: Int -> a

data Object = forall a. Object a
data Ordered = forall a. Ord a => Ordered a

-- note 1. no way to have specific b for Worker
-- note 2. no need to worry about monomorphization restriction.
-- note 3. you can only work with buffer interface, you can't use
--         specific type anymore.
data Worker x y = forall a. Buffer a => Worker { buffer :: a
                                               , input  :: x
                                               , output :: y
                                               }
workerFoo :: Buffer b => b -> Worker Int Int
workerFoo b = Worker b 10 10

-- need existential type because b doesn't presnt on Val branch.
data Expr a = Val a | forall b. Apply (Expr (b -> a)) (Expr b)

-- need exitential type to make sure the io ref is the one used in the closure.
data Action = forall b. Act (IORef b) (b -> IO ())

-- dynamic dispatching --

class Shape_ a where
  perimeter :: a -> Double
  area :: a -> Double

data Shape = forall a. Shape_ a => Shape a

type Radius = Double
type Side = Double

data Circle = Circle Radius
data Rectangle = Rectangle Side Side
data Square = Square Side

instance Shape_ Circle where
  perimeter (Circle r) = 2 * pi * r
  area (Circle r) = pi * r * r

instance Shape_ Rectangle where
  perimeter (Rectangle x y) = 2 * (x + y)
  area (Rectangle x y) = x * y

instance Shape_ Square where
  perimeter (Square s) = 4 * s
  area (Square s) = s * s

instance Shape_ Shape where
  perimeter (Shape shape) = perimeter shape
  area (Shape shape) = area shape

c1 = Circle 10.1
r1 = Rectangle 10.1 10.1
s1 = Square 12

shapes :: [Shape]
shapes = [Shape c1, Shape r1, Shape s1]

perimeters = perimeter <$> shapes
areas = area <$> shapes

-- alternative method --
type Point = (Float, Float)


-- exmaple use case in raytracer--
class BoundingShape_ a where
  volume :: a -> Double

data BoundingShape = forall a. BoundingShape_ a => BoundingShape a

data BoundingBox = BoundingBox Double Double Double deriving Show
data BoundingSphere = BoundingSphere Double deriving Show

instance BoundingShape_ BoundingBox where
  volume (BoundingBox x y z) = x * y * z

instance BoundingShape_ BoundingSphere where
  volume (BoundingSphere r) = (4 * pi * r^3) / fromIntegral 3

type Fragment = Double

-- use bounding sphere
class Renderable a where
  boundingSphere :: a -> BoundingSphere
  hit :: a -> [Fragment]

-- hide all renderable behind AnyRenderable. Now we can but all renderable into one
-- container and render all of them together.
data AnyRenderable = forall a. Renderable a => AnyRenderable a

instance Renderable AnyRenderable where
  boundingSphere (AnyRenderable a) = boundingSphere a
  hit (AnyRenderable a) = hit a

data Cube3D = Cube3D Double Double Double
data Ball3D = Ball3D Double

-- implementation doesn't really matter here.
instance Renderable Cube3D where
  boundingSphere (Cube3D x y z) = BoundingSphere (maximum [x, y, z])
  hit (Cube3D x y z) = [x, y, z]

instance Renderable Ball3D where
  boundingSphere (Ball3D r) = BoundingSphere r
  hit (Ball3D r) = [r, 1, 2, 3]

objsToRender = [ AnyRenderable (Cube3D 1 2 3)
               , AnyRenderable (Cube3D 2 3 4)
               , AnyRenderable (Ball3D 39)
               , AnyRenderable (Ball3D 23)
               ]

render1 = boundingSphere <$> objsToRender
render2 = hit <$> objsToRender


-- Another exercise --
class Animal_ a where
  move :: a -> IO ()

data Animal = forall a. Animal_ a => Animal a
instance Animal_ Animal where
  move (Animal a) = move a

class Animal_ a => Cat_ a where
  meow :: a -> IO ()

data Cat = forall a. Cat_ a => Cat a
instance Animal_ Cat where
  move (Cat a) = move a
instance Cat_ Cat where
  meow (Cat a) = meow a

class Animal_ a => Dog_ a where
  bark :: a -> IO ()
data Dog = forall a. Dog_ a => Dog a

instance Animal_ Dog where
  move (Dog a) = move a
instance Dog_ Dog where
  bark (Dog a) = bark a

data Persian = Persian
instance Animal_ Persian where
  move _ = putStrLn "Persian the cat: Moving"
instance Cat_ Persian where
  meow _ = putStrLn "Persian the cat: Meow"

data Corgi = Corgi
instance Animal_ Corgi where
  move _ = putStrLn "Corgi the dog: Moving"
instance Dog_ Corgi where
  bark _ = putStrLn "Corgi the dog: woof woof"

persian = Persian
corgi1 = Corgi
corgi2 = Corgi

moveAnimals = sequence_ $ move <$>  [Animal persian, Animal corgi1, Animal corgi2]
moveDogs = sequence_ $ bark <$> [Dog corgi1, Dog corgi2]
