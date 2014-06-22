module Data.Monoid.Sum where

import Data.Monoid

data Sum = Sum Number

runSum :: Sum -> Number
runSum (Sum x) = x

instance eqSum :: Eq Sum where
  (==) (Sum x) (Sum y) = x == y
  (/=) (Sum x) (Sum y) = x /= y
  
instance ordSum :: Ord Sum where
  compare (Sum x) (Sum y) = compare x y

instance showSum :: Show Sum where
  show (Sum a) = "Sum (" ++ show a ++ ")"

instance semigroupSum :: Semigroup Sum where
  (<>) (Sum x) (Sum y) = Sum (x + y)

instance monoidSum :: Monoid Sum where
  mempty = Sum 0
