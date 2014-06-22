module Data.Monoid.First where

import Data.Maybe
import Data.Monoid

data First a = First (Maybe a)

runFirst :: forall a. First a -> Maybe a
runFirst (First m) = m

instance eqFirst :: (Eq a) => Eq (First a) where
  (==) (First x) (First y) = x == y
  (/=) (First x) (First y) = x /= y
  
instance ordFirst :: (Ord a) => Ord (First a) where
  compare (First x) (First y) = compare x y

instance showFirst :: (Show a) => Show (First a) where
  show (First a) = "First (" ++ show a ++ ")"

instance semigroupFirst :: Semigroup (First a) where
  (<>) first@(First (Just _)) _ = first
  (<>) _ second = second

instance monoidFirst :: Monoid (First a) where
  mempty = First Nothing
