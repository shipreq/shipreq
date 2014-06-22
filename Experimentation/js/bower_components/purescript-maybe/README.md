# Module Documentation

## Module Data.Maybe

### Types

    data Maybe a where
      Nothing :: Maybe a
      Just :: a -> Maybe a


### Type Class Instances

    instance alternativeMaybe :: Alternative Maybe

    instance applicativeMaybe :: Applicative Maybe

    instance applyMaybe :: Apply Maybe

    instance bindMaybe :: Bind Maybe

    instance eqMaybe :: (Eq a) => Eq (Maybe a)

    instance functorMaybe :: Functor Maybe

    instance monadMaybe :: Monad Maybe

    instance ordMaybe :: (Ord a) => Ord (Maybe a)

    instance showMaybe :: (Show a) => Show (Maybe a)


### Values

    fromMaybe :: forall a. a -> Maybe a -> a

    isJust :: forall a. Maybe a -> Prim.Boolean

    isNothing :: forall a. Maybe a -> Prim.Boolean

    maybe :: forall a b. b -> (a -> b) -> Maybe a -> b


## Module Data.Maybe.Unsafe

### Values

    fromJust :: forall a. Maybe a -> a