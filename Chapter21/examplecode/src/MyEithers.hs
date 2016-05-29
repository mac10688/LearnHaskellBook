module MyEithers where

data MyEither a b =
      MyLeft a
    | MyRight b
    deriving (Eq, Ord, Show)

instance Functor (MyEither a) where
    fmap _ (MyLeft x) = MyLeft x
    fmap f (MyRight y) = MyRight (f y)

instance Applicative (MyEither e) where
    pure = MyRight
    MyLeft e <*> _ = MyLeft e
    MyRight f <*> r = fmap f r

instance Foldable (MyEither a) where
    foldMap _ (MyLeft _) = mempty
    foldMap f (MyRight y) = f y

    foldr _ z (MyLeft _) = z
    foldr f z (MyRight y) = f y z

instance Traversable (MyEither a) where
    traverse _ (MyLeft x) = pure (MyLeft x)
    traverse f (MyRight y) = MyRight <$> f y
