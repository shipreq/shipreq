module Data.Either where

data Either a b = Left a | Right b

either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a) = f a
either _ g (Right b) = g b

isLeft :: forall a b. Either a b -> Boolean
isLeft = either (const true) (const false)

isRight :: forall a b. Either a b -> Boolean
isRight = either (const false) (const true)

instance functorEither :: Functor (Either a) where
  (<$>) _ (Left x) = Left x
  (<$>) f (Right y) = Right (f y)

instance applyEither :: Apply (Either e) where
  (<*>) (Left e) _ = Left e
  (<*>) (Right f) r = f <$> r

instance applicativeEither :: Applicative (Either e) where
  pure = Right

instance bindEither :: Bind (Either e) where
  (>>=) = either (\e _ -> Left e) (\a f -> f a)

instance monadEither :: Monad (Either e)

instance showEither :: (Show a, Show b) => Show (Either a b) where
  show (Left x) = "Left (" ++ show x ++ ")"
  show (Right y) = "Right (" ++ show y ++ ")"

instance eqEither :: (Eq a, Eq b) => Eq (Either a b) where
  (==) (Left a1)  (Left a2)  = a1 == a2
  (==) (Right b1) (Right b2) = b1 == b2
  (==) _          _          = false
  (/=) a          b          = not (a == b)

instance ordEither :: (Ord a, Ord b) => Ord (Either a b) where
  compare (Left a1)  (Left a2)  = compare a1 a2
  compare (Right b1) (Right b2) = compare b1 b2
  compare (Left a)   _          = LT
  compare _          (Left b)   = GT
