module Data.Monoid.Any where

import Data.Monoid

data Any = Any Boolean

runAny :: Any -> Boolean
runAny (Any x) = x

instance eqAny :: Eq Any where
  (==) (Any x) (Any y) = x == y
  (/=) (Any x) (Any y) = x /= y
  
instance showAny :: Show Any where
  show (Any a) = "Any (" ++ show a ++ ")"

instance semigroupAny :: Semigroup Any where
  (<>) (Any x) (Any y) = Any (x || y)

instance monoidAny :: Monoid Any where
  mempty = Any false
