module Types.Types where

import Data.List (List)
import Data.Tuple (Tuple)
import Prelude (class Monoid, class Ord, unit)
import Unsafe.Coerce (unsafeCoerce)


-- rewrite some useful functions.

class Functor' f where
    fmap' :: forall a b. (a -> b) -> f a -> f b
    voidLeft' :: forall a b. a -> f b -> f a

class Functor' f <= Applicative' f where
    pure' :: forall a.  a -> f a
    apply' :: forall a b.  f (a -> b) -> f a -> f b
    applyFirst' :: forall a b. f a -> f b -> f a
    applySecond' :: forall a b. f a -> f b -> f b

class Applicative' m <= Monad' m where
    return' :: forall a. a -> m a
    ap' :: forall a b. m (a -> b) -> m a -> m b
    bind' :: forall a b. m a -> (a -> m b) -> m b
    join' :: forall a. m (m a) -> m a
    liftM' :: forall a b. (a -> b) -> m a -> m b            -- functor
    mapM' :: forall a b. (a -> m b) -> List a -> m (List b) -- traverse for App
    kleisli' :: forall a b c. (a -> m b) -> (b -> m c) -> a -> m c

class MonadTrans' t where
    lift' :: forall m a. Monad' m => m a -> t m a

class Semigroup' a where
    mappend' :: a -> a -> a

-- *
class Semigroup' a <= Monoid' a where
    mempty' :: a
    mconcat' :: List a -> a

class Applicative' f <= Alternative' f where
    empty' :: forall a. f a
    alt' :: forall a. f a -> f a -> f a
    some' :: forall a. f a -> f (List a)
    many' :: forall a. f a -> f (List a)

class Monad' m <= MonadPlus' m where
    mzero' :: forall a. m a
    mplus' :: forall a. m a -> m a -> m a

-- * -> *
class Foldable' t where
    foldr' :: forall a b. (a -> b -> b) -> b -> t a -> b
    foldl' :: forall a b. (b -> a -> b) -> b -> t a -> b
    foldMap' :: forall a m. (Monoid m) => (a -> m) -> t a -> m

    toList' :: forall a. t a -> List a
    null' :: forall a. t a -> Boolean
    elem' :: forall a. (Ord a) => a -> t a -> Boolean
    maximum' :: forall a. (Ord a) =>  t a -> a
    minimum' :: forall a. (Ord a) =>  t a -> a

-- * -> *
class (Functor' t, Foldable' t) <= Traversable' t where
    traverse :: forall a b f. Applicative' f => (a -> f b) -> t a -> f (t b)
    sequenceA :: forall a f. Applicative' f => t (f a) -> f (t a)

-- * -> * -> *
class Bifunctor' p where
    bimap' :: forall a b c d. (a -> b) -> (c -> d) -> p a c -> p b d
    bifirst' :: forall a b c. (a -> b) -> p a c -> p b c
    bisecond' :: forall a b c. (b -> c) -> p a b -> p a c

-- * -> * -> *
class Category' arr where
    id' :: forall a. arr a a
    cdot :: forall a b c. arr b c -> arr a b -> arr a c

class Category' arr <= Arrow' arr where
    arr' :: forall b c. (b -> c) -> (arr b c)
    arrfirst' :: forall b c d. (arr b c) -> (arr (Tuple b d) (Tuple c d))
    arrsecond' :: forall b c d. (arr b c) -> (arr (Tuple d b) (Tuple d c))
    arrddd' :: forall b c b' c'. (arr b c) -> (arr b' c')
            -> (arr (Tuple b b') (Tuple c c'))
    arraaa' :: forall b c b' c'. (arr b c) -> (arr b' c')
            -> (arr b (Tuple c c'))

zip' :: forall a b. List a -> List b -> List (Tuple a b)
zip' = unsafeCoerce unit

zipWith' :: forall a b c. (a -> b -> c) -> List a -> List b -> List c
zipWith' = unsafeCoerce unit

concat' :: forall a t. (Foldable' t) => t (List a) -> List a
concat' = unsafeCoerce unit

concatMap' :: forall a b t. (Foldable' t) => (a -> List b) -> t a -> List b
concatMap' = unsafeCoerce unit

compose' :: forall a b c. (b -> c) -> (a -> b) -> a -> c
compose' = unsafeCoerce unit

dollar' :: forall a b. (a -> b) -> a -> b
dollar' = unsafeCoerce unit

nub' :: forall a. List a -> List a
nub' = unsafeCoerce unit
