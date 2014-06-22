module Data.Monoid.All where

import Data.Monoid

data All = All Boolean

runAll :: All -> Boolean
runAll (All x) = x

instance eqAll :: Eq All where
  (==) (All x) (All y) = x == y
  (/=) (All x) (All y) = x /= y
  
instance showAll :: Show All where
  show (All a) = "All (" ++ show a ++ ")"

instance semigroupAll :: Semigroup All where
  (<>) (All x) (All y) = All (x && y)

instance monoidAll :: Monoid All where
  mempty = All true
