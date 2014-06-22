module Data.Monoid.Product where

import Data.Monoid

data Product = Product Number

runProduct :: Product -> Number
runProduct (Product x) = x

instance eqProduct :: Eq Product where
  (==) (Product x) (Product y) = x == y
  (/=) (Product x) (Product y) = x /= y
  
instance ordProduct :: Ord Product where
  compare (Product x) (Product y) = compare x y

instance showProduct :: Show Product where
  show (Product a) = "Product (" ++ show a ++ ")"

instance semigroupProduct :: Semigroup Product where
  (<>) (Product x) (Product y) = Product (x * y)

instance monoidProduct :: Monoid Product where
  mempty = Product 1
