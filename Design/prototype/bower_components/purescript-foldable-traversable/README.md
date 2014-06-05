# Module Documentation

## Module Data.Foldable

### Type Classes

    class Foldable f where
      foldr :: forall a b. (a -> b -> b) -> b -> f a -> b
      foldl :: forall a b. (b -> a -> b) -> b -> f a -> b
      foldMap :: forall a m. (Monoid m) => (a -> m) -> f a -> m


### Type Class Instances

    instance foldableArray :: Foldable Prim.Array

    instance foldableEither :: Foldable (Either a)

    instance foldableMaybe :: Foldable Maybe

    instance foldableRef :: Foldable Ref

    instance foldableTuple :: Foldable (Tuple a)


### Values

    all :: forall a f. (Foldable f) => (a -> Prim.Boolean) -> f a -> Prim.Boolean

    and :: forall f. (Foldable f) => f Prim.Boolean -> Prim.Boolean

    any :: forall a f. (Foldable f) => (a -> Prim.Boolean) -> f a -> Prim.Boolean

    elem :: forall a f. (Eq a, Foldable f) => a -> f a -> Prim.Boolean

    find :: forall a f. (Foldable f) => (a -> Prim.Boolean) -> f a -> Maybe a

    fold :: forall f m. (Foldable f, Monoid m) => f m -> m

    foldlArray :: forall a b. (b -> a -> b) -> b -> [a] -> b

    foldrArray :: forall a b. (a -> b -> b) -> b -> [a] -> b

    for_ :: forall a b f m. (Functor m, Applicative m, Foldable f) => f a -> (a -> m b) -> m {  }

    lookup :: forall a b f. (Eq a, Foldable f) => a -> f (Tuple a b) -> Maybe b

    mconcat :: forall f m. (Foldable f, Monoid m) => f m -> m

    notElem :: forall a f. (Eq a, Foldable f) => a -> f a -> Prim.Boolean

    or :: forall f. (Foldable f) => f Prim.Boolean -> Prim.Boolean

    product :: forall f. (Foldable f) => f Prim.Number -> Prim.Number

    sequence_ :: forall a f m. (Functor m, Applicative m, Foldable f) => f (m a) -> m {  }

    sum :: forall f. (Foldable f) => f Prim.Number -> Prim.Number

    traverse_ :: forall a b f m. (Functor m, Applicative m, Foldable f) => (a -> m b) -> f a -> m {  }


## Module Data.Traversable

### Type Classes

    class Traversable t where
      traverse :: forall a b m. (Functor m, Applicative m) => (a -> m b) -> t a -> m (t b)
      sequence :: forall a m. (Functor m, Applicative m) => t (m a) -> m (t a)


### Type Class Instances

    instance traversableArray :: Traversable Prim.Array

    instance traversableEither :: Traversable (Either a)

    instance traversableMaybe :: Traversable Maybe

    instance traversableRef :: Traversable Ref

    instance traversableTuple :: Traversable (Tuple a)


### Values

    for :: forall a b m t. (Functor m, Applicative m, Traversable t) => t a -> (a -> m b) -> m (t b)

    zipWithA :: forall m a b c. (Functor m, Applicative m) => (a -> b -> m c) -> [a] -> [b] -> m [c]