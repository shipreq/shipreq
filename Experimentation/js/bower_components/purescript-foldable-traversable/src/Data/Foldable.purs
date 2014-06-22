module Data.Foldable where

import Prelude
import Control.Apply
import Data.Either
import Data.Eq
import Data.Maybe
import Data.Monoid
import Data.Monoid.First
import Data.Tuple

class Foldable f where
  foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
  foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
  foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m

instance foldableArray :: Foldable [] where
  foldr f z xs = foldrArray f z xs

  foldl f z xs = foldlArray f z xs

  foldMap f xs = foldr (\x acc -> f x <> acc) mempty xs

instance foldableEither :: Foldable (Either a) where
  foldr _ z (Left _)  = z
  foldr f z (Right x) = x `f` z

  foldl _ z (Left _)  = z
  foldl f z (Right x) = z `f` x

  foldMap f (Left _)  = mempty
  foldMap f (Right x) = f x

instance foldableMaybe :: Foldable Maybe where
  foldr _ z Nothing  = z
  foldr f z (Just x) = x `f` z

  foldl _ z Nothing  = z
  foldl f z (Just x) = z `f` x

  foldMap f Nothing  = mempty
  foldMap f (Just x) = f x

instance foldableRef :: Foldable Ref where
  foldr f z (Ref x) = x `f` z

  foldl f z (Ref x) = z `f` x

  foldMap f (Ref x) = f x

instance foldableTuple :: Foldable (Tuple a) where
  foldr f z (Tuple _ x) = x `f` z

  foldl f z (Tuple _ x) = z `f` x

  foldMap f (Tuple _ x) = f x

fold :: forall f m. (Foldable f, Monoid m) => f m -> m
fold = foldMap id

traverse_ :: forall a b f m. (Functor m, Applicative m, Foldable f) => (a -> m b) -> f a -> m {}
traverse_ f = foldr ((*>) <<< f) (pure {})

for_ :: forall a b f m. (Functor m, Applicative m, Foldable f) => f a -> (a -> m b) -> m {}
for_ = flip traverse_

sequence_ :: forall a f m. (Functor m, Applicative m, Foldable f) => f (m a) -> m {}
sequence_ = traverse_ id

mconcat :: forall f m. (Foldable f, Monoid m) => f m -> m
mconcat = foldl (<>) mempty

and :: forall f. (Foldable f) => f Boolean -> Boolean
and = foldl (&&) true

or :: forall f. (Foldable f) => f Boolean -> Boolean
or = foldl (||) false

any :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean
any p = or <<< foldMap (\x -> [p x])

all :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Boolean
all p = and <<< foldMap (\x -> [p x])

sum :: forall f. (Foldable f) => f Number -> Number
sum = foldl (+) 0

product :: forall f. (Foldable f) => f Number -> Number
product = foldl (*) 1

elem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean
elem = any  <<< (==)

notElem :: forall a f. (Eq a, Foldable f) => a -> f a -> Boolean
notElem x = not <<< elem x

find :: forall a f. (Foldable f) => (a -> Boolean) -> f a -> Maybe a
find p f = case foldMap (\x -> if p x then [x] else []) f of
  (x:_) -> Just x
  []    -> Nothing
 
lookup :: forall a b f. (Eq a, Foldable f) => a -> f (Tuple a b) -> Maybe b
lookup a f = runFirst $ foldMap (\(Tuple a' b) -> First (if a == a' then Just b else Nothing)) f

foreign import foldrArray
  "function foldrArray(f) {\
  \  return function(z) {\
  \    return function(xs) {\
  \      var acc = z;\
  \      for (var i = xs.length - 1; i >= 0; --i) {\
  \        acc = f(xs[i])(acc);\
  \      }\
  \      return acc;\
  \    }\
  \  }\
  \}" :: forall a b. (a -> b -> b) -> b -> [a] -> b

foreign import foldlArray
  "function foldlArray(f) {\
  \  return function(z) {\
  \    return function(xs) {\
  \      var acc = z;\
  \      for (var i = 0, len = xs.length; i < len; ++i) {\
  \        acc = f(acc)(xs[i]);\
  \      }\
  \      return acc;\
  \    }\
  \  }\
  \}" :: forall a b. (b -> a -> b) -> b -> [a] -> b
