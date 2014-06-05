# Module Documentation

## Module Data.Either

### Types

    data Either a b where
      Left :: a -> Either a b
      Right :: b -> Either a b


### Type Class Instances

    instance applicativeEither :: Applicative (Either e)

    instance applyEither :: Apply (Either e)

    instance bindEither :: Bind (Either e)

    instance eqEither :: (Eq a, Eq b) => Eq (Either a b)

    instance functorEither :: Functor (Either a)

    instance monadEither :: Monad (Either e)

    instance ordEither :: (Ord a, Ord b) => Ord (Either a b)

    instance showEither :: (Show a, Show b) => Show (Either a b)


### Values

    either :: forall a b c. (a -> c) -> (b -> c) -> Either a b -> c

    isLeft :: forall a b. Either a b -> Prim.Boolean

    isRight :: forall a b. Either a b -> Prim.Boolean