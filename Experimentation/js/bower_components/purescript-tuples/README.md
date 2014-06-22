# Data.Tuple

### Types

    data Tuple a b where
      Tuple :: a -> b -> Tuple a b

### Type Class Instances

    instance applicativeTuple :: (Monoid a) => Applicative (Tuple a)

    instance applyTuple :: (Semigroup a) => Apply (Tuple a)

    instance bindTuple :: (Semigroup a) => Bind (Tuple a)

    instance eqTuple :: (Eq a, Eq b) => Eq (Tuple a b)

    instance functorTuple :: Functor (Tuple a)

    instance monadTuple :: (Monoid a) => Monad (Tuple a)

    instance ordTuple :: (Ord a, Ord b) => Ord (Tuple a b)

    instance showTuple :: (Show a, Show b) => Show (Tuple a b)

### Values

    curry :: forall a b c. (Tuple a b -> c) -> a -> b -> c

    fst :: forall a b. Tuple a b -> a

    snd :: forall a b. Tuple a b -> b

    swap :: forall a b. Tuple a b -> Tuple b a

    uncurry :: forall a b c. (a -> b -> c) -> Tuple a b -> c

    unzip :: forall a b. [Tuple a b] -> Tuple [a] [b]

    zip :: forall a b. [a] -> [b] -> [Tuple a b]
