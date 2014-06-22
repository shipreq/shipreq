module Data.Monoid.Dual where

import Data.Monoid

data Dual a = Dual a

runDual :: forall a. Dual a -> a
runDual (Dual x) = x

instance eqDual :: (Eq a) => Eq (Dual a) where
  (==) (Dual x) (Dual y) = x == y
  (/=) (Dual x) (Dual y) = x /= y
  
instance ordDual :: (Ord a) => Ord (Dual a) where
  compare (Dual x) (Dual y) = compare x y

instance showDual :: (Show a) => Show (Dual a) where
  show (Dual a) = "Dual (" ++ show a ++ ")"

instance semigroupDual :: (Semigroup a) => Semigroup (Dual a) where
  (<>) (Dual x) (Dual y) = Dual (y <> x)

instance monoidDual :: (Monoid a) => Monoid (Dual a) where
  mempty = Dual mempty
