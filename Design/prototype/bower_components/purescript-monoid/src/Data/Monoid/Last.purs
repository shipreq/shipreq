module Data.Monoid.Last where

import Data.Maybe
import Data.Monoid

data Last a = Last (Maybe a)

runLast :: forall a. Last a -> Maybe a
runLast (Last m) = m

instance eqLast :: (Eq a) => Eq (Last a) where
  (==) (Last x) (Last y) = x == y
  (/=) (Last x) (Last y) = x /= y
  
instance ordLast :: (Ord a) => Ord (Last a) where
  compare (Last x) (Last y) = compare x y

instance showLast :: (Show a) => Show (Last a) where
  show (Last a) = "Last (" ++ show a ++ ")"

instance semigroupLast :: Semigroup (Last a) where
  (<>) _ last@(Last (Just _)) = last
  (<>) last (Last Nothing)    = last

instance monoidLast :: Monoid (Last a) where
  mempty = Last Nothing
