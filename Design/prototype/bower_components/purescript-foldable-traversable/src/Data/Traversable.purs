module Data.Traversable where

import Prelude
import Data.Array (zipWith)
import Data.Either
import Data.Eq
import Data.Foldable
import Data.Maybe
import Data.Tuple

class Traversable t where
  traverse :: forall a b m. (Functor m, Applicative m) => (a -> m b) -> t a -> m (t b)
  sequence :: forall a m. (Functor m, Applicative m) => t (m a) -> m (t a)

instance traversableArray :: Traversable [] where
  traverse _ []     = pure []
  traverse f (x:xs) = (:) <$> (f x) <*> traverse f xs

  sequence []     = pure []
  sequence (x:xs) = (:) <$> x <*> sequence xs

instance traversableEither :: Traversable (Either a) where
  traverse _ (Left x)  = pure (Left x)
  traverse f (Right x) = Right <$> f x

  sequence (Left x) = pure (Left x)
  sequence (Right x)  = Right <$> x

instance traversableRef :: Traversable Ref where
  traverse f (Ref x) = Ref <$> f x

  sequence (Ref x) = Ref <$> x

instance traversableMaybe :: Traversable Maybe where
  traverse _ Nothing  = pure Nothing
  traverse f (Just x) = Just <$> f x

  sequence Nothing  = pure Nothing
  sequence (Just x) = Just <$> x

instance traversableTuple :: Traversable (Tuple a) where
  traverse f (Tuple x y) = Tuple x <$> f y

  sequence (Tuple x y) = Tuple x <$> y

for :: forall a b m t. (Functor m, Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)
for x f = traverse f x

zipWithA :: forall m a b c. (Functor m, Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithA f xs ys = sequence (zipWith f xs ys)

