module Demo where


import qualified Control.Applicative as A
import ReadingComprehension

newtype HumanName =
  HumanName String
  deriving (Eq, Show)

newtype DogName =
  DogName String
  deriving (Eq, Show)

newtype Address =
  Address String
  deriving (Eq, Show)


data Person =
  Person {
    humanName :: HumanName
  , dogName :: DogName
  , address :: Address
  } deriving (Eq, Show)


data Dog =
  Dog {
    dogsName :: DogName
  , dogsAddress :: Address
  } deriving (Eq, Show)


pers :: Person
pers =
  Person (HumanName "Big Bird")
         (DogName "Barkley")
         (Address "Sesame Street")


jimmy :: Person
jimmy =
  Person (HumanName "Jimmy")
         (DogName "Iggy")
         (Address "Nantong")


-- without reader
getDog :: Person -> Dog
getDog p =
  Dog (dogName p) (address p)


-- with reader
getDogR :: Person -> Dog
getDogR =
  Dog <$> dogName <*> address


-- with reader using liftA2
getDogR' :: Person -> Dog
getDogR' = A.liftA2 Dog dogName address


-- with function monad
getDogM :: Person -> Dog
getDogM =
  dogName >>=
    \name ->
      address >>=
        \a ->
          return $ Dog name a

getDogM' :: Person -> Dog
getDogM' = do
  name <- dogName
  addr <- address
  return $ Dog name addr

-- Reader' wrapper of dogName and address
dnReader :: Reader' Person DogName
dnReader = Reader' dogName

addrReader :: Reader' Person Address
addrReader = Reader' address

getDogRM :: Person -> Dog
getDogRM =
  runReader' dnReader >>=
    \name ->
      runReader' addrReader >>=
        \addr ->
          return $ Dog name addr

-- 22.7 Monad of functions
foo :: (Functor f, Num a) => f a -> f a
foo = fmap (+1)

bar :: Foldable f => t -> f a -> (t, Int)
bar r t = (r, length t)

-- one function do both
froot :: Num a => [a] -> ([a], Int)
froot r = (map (+1) r, length r)

-- or we can combine two functions we already have.
-- Note class (Functor f, Foldable f) => (Traversable t :: * -> *)
barPlus :: (Traversable t, Num a) => t a -> (t a, Int)
barPlus r = (foo r, length r)

-- or
frooty :: Num a => [a] -> ([a], Int)
frooty r = bar (foo r) r

-- now try to write in a more reader like way
frooty' :: Num a => [a] -> ([a], Int)
frooty' = \r -> bar (foo r) r

-- now abstract function out
fooBind :: (r -> a) -> (a -> r -> b) -> (r -> b)
-- fooBind m k = \r -> k (m r) r
fooBind m k r = k (m r) r

-- compare with bind
-- (>>=) :: Monad m => m a -> (a -> m b) -> m b

