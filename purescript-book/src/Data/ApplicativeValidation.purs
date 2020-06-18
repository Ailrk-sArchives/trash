module Data.ValidationApplicative where

import Control.Apply (lift2)
import Data.AddressBook (Address, Person, PhoneType(..), address, person, phoneNumber, PhoneNumber(..))
import Data.BooleanAlgebra ((&&))
import Data.Bounded (Ordering(..))
import Data.Either (Either(..))
import Data.Eq (class Eq, eq)
import Data.Foldable (class Foldable, foldMap, foldr, foldl)
import Data.List (List(..), fromFoldable)
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.Show (class Show)
import Data.String as S
import Data.String.Regex as R
import Data.String.Regex.Flags (noFlags)
import Data.Traversable (class Traversable, sequence, traverse)
import Data.Validation.Semigroup (V, invalid)
import Partial.Unsafe (unsafePartial)
import Prelude (class Applicative, class Functor, class Semiring, Unit, apply, identity, map, pure, show, unit, (*>), (+), (/=), (<$>), (<*>), (<>))


-- pure takes a value and returns a value whose type ahs been wrapped with
-- the type constructor f.
-- pure can be thought as a lifting function of zero argument.

-- intuition
-- applicative helps us work with contexts (sideeffects.)
-- if `f` represents some larger programming language with side
-- effects, the `Apply` and `Applicative` allows to lift value
-- from purescipt into the larger language.
-- applicative generalize the notion of function application to
-- type constuctor.

-- if a value cannot be expressed as pure x for some x, then it
-- represent a term only exists in the larger language.
-- for instance, Nothing cannot be achieve by using pure, thus
-- it enlarge purescript as representing a missing term.
fullname :: String -> String -> String -> String
fullname first middle last = last <> ", " <> first <> "," <> middle

checkfullname :: Maybe String -> Maybe String -> Maybe String -> Maybe String
checkfullname f m l = fullname <$> f <*> m <*> l

-- applicative do
checkfullname' :: Maybe String -> Maybe String -> Maybe String -> Maybe String
checkfullname' f m l = ado
    f' <- f
    m' <- m
    l' <- l
    in fullname f' m' l'

-- turn maybe to either.
withError :: forall a. Maybe a -> String -> Either String a
withError Nothing err = Left err
withError (Just a) _ = Right a

-- fullname restrict the type of Either String String
-- if first three args are Maybe a, if they are Just a
-- a could be any aribitrary types and could be posibly
-- incompatible with fullname's type signature.
fullNameEither :: Maybe String -> Maybe String -> Maybe String -> Either String String
fullNameEither f m l =
    fullname <$> (f `withError` "First name is missing")
             <*> (m `withError` "Second name is missing")
             <*> (l `withError` "Thrid name is missing")

-- combining effects.
-- think in terms of types make this sort of reasoning easy.
combineList :: forall f a. Applicative f => List (f a) -> f (List a)
combineList Nil = pure Nil
combineList (Cons x xs) = Cons <$> x <*> combineList xs

combined :: Maybe (List Int)
combined = combineList (fromFoldable [Just 1, Just 2, Just 3])

addMaybe :: forall a. Semiring a => Maybe a -> Maybe a -> Maybe a
addMaybe = lift2 (\a b -> a + b)

-- validation
type Errors = Array String

-- manully write regex
nonEmpty :: String -> String -> V Errors Unit
nonEmpty field "" = invalid ["Field"
    <> field
    <> "Filed cannot be empty"
    <> " cannot be empty."]
nonEmpty _ _ = pure unit

lengthIs :: String -> Int -> String -> V Errors Unit
lengthIs field len value | S.length value /= len =
    invalid ["Field" <> field <> " must have length " <> show len]
lengthIs _ _ _ = pure unit

validateAddress :: Address -> V Errors Address
validateAddress a =
    address <$> ("Street" `nonEmpty` a.street *> pure a.street)
            <*> ("City" `nonEmpty` a.city *> pure a.city)
            <*> (lengthIs "State" 2 a.state *> pure a.state)

-- regular expressoin validator.
phoneNumberRegex :: R.Regex
phoneNumberRegex = unsafePartial
   case  R.regex "^\\d{3}-\\d{3}-\\d{4}$" noFlags of
        Right r -> r


matches :: String -> R.Regex -> String -> V Errors Unit
matches _ regex value | R.test regex value = pure unit
matches field _ _ = invalid ["Field " <> field <> " did not match the required format"]

validatePhoneNumber :: PhoneNumber -> V Errors PhoneNumber
validatePhoneNumber pn =
    phoneNumber <$> pure pn.phoneType
                <*> (matches "Number"
                        phoneNumberRegex
                        pn.phoneNumber
                    *> pure pn.phoneNumber)

nonEmptyRegex :: R.Regex
nonEmptyRegex = unsafePartial
    case R.regex "^\\s\\s*" noFlags of
         Right r -> r

stateRegex :: R.Regex
stateRegex = unsafePartial
    case R.regex "[A-Z]{2}" noFlags of
         Right r -> r

validateAddressImproved :: Address -> V Errors Address
validateAddressImproved addr =
    address <$> (matches "City " nonEmptyRegex addr.city *> pure addr.city)
            <*> (matches "Street " nonEmptyRegex addr.street *> pure addr.street)
            <*> (matches "State " stateRegex addr.state *> pure addr.street)


-- traversable functors.
arrayNonEmpty :: forall a. String -> Array a -> V Errors Unit
arrayNonEmpty field [] =
    invalid ["Field " <> field <> "must contain at least one value"]
arrayNonEmpty _ _ = pure unit

-- *> is the sequence of applicative.
-- traverse :: forall m t. Traversable t => Applicative m =>
--             (a -> m b) -> t a -> m (t b)
-- traverse walks over the elements of data structure, performing
-- computation with side effects and accumulating a result.

validatePerson :: Person -> V Errors Person
validatePerson p =
    person <$> ("firstname" `nonEmpty` p.firstname *> pure p.firstname)
           <*> ("lastname" `nonEmpty` p.lastname *> pure p.lastname)
           <*> validateAddress p.address
           <*> (arrayNonEmpty "Phone Numberes" p.phoneNumbers *>
               traverse validatePhoneNumber p.phoneNumbers)

-- sequence :: forall a m. Applicative m => t (m a) => m (t a)

data Tree a = Leaf | Branch (Tree a) a (Tree a)

instance eqTree :: (Eq a) => Eq (Tree a) where
    eq Leaf Leaf = true
    eq (Branch _ _ _) Leaf = false
    eq Leaf (Branch _ _ _) = false
    eq (Branch tl val tr) (Branch tl' val' tr') =
        (eq val val') && (eq tl tl') && (eq tr tr')

instance showTree :: (Show a) => Show (Tree a) where
    show Leaf = "Leaf"
    show (Branch tl val tr) = show tl <> show val <> show tr

instance functorTree :: Functor Tree where
    map f Leaf = Leaf
    map f (Branch tl val tr) = Branch (f <$> tl) (f val) (f <$> tr)

-- foldable for tree.
-- foldr :: (a -> b -> b) -> b -> f a -> f b
-- foldl :: (b -> a -> b) -> b -> f a -> f b
-- foldMap :: (a -> m) -> f a ->  m
instance foldableTree :: Foldable Tree where
    foldr _ x Leaf = x
    foldr f x (Branch tl val tr) = foldr f (f val (foldr f x tr)) tl
    foldl _ x Leaf = x
    foldl f x (Branch tl val tr) = foldl f (f (foldl f x tl) val) tr
    foldMap _ Leaf = mempty
    foldMap f (Branch tl val tr) = foldMap f tl <> f val <> foldMap f tr

-- traverse :: forall m t a b. (Traversable t) => (Applicative m)
--             (a -> m b) -> t a -> m (t b)
-- sequence :: forall m t a b. (Traversable t) => (Applicative m)
--             t (m a) -> m (t a)
instance inorderTraversableTree :: Traversable Tree where
    traverse _ Leaf = pure Leaf
    traverse f (Branch tl val tr) = Branch <$> traverse f tl <*> f val <*> traverse f tr
    sequence = traverse identity

tree1 :: Tree String
tree1 = Branch
    (Branch
        (Branch Leaf "1" Leaf)
        "2"
        (Branch
            Leaf
            "3"
            (Branch
                Leaf
                "4"
                (Branch
                    Leaf
                        "5" Leaf))))
    "6"
    (Branch Leaf "7" Leaf)
