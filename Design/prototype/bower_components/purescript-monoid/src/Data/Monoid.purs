module Data.Monoid where

import Data.Array ()

class (Semigroup m) <= Monoid m where
  mempty :: m

instance monoidString :: Monoid String where
  mempty = ""

instance monoidArray :: Monoid [a] where
  mempty = []
  
instance monoidUnit :: Monoid Unit where
  mempty = unit
