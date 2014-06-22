module Data.Maybe.Unsafe where

import Data.Maybe

fromJust :: forall a. Maybe a -> a
fromJust (Just x) = x
