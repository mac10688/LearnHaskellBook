-- | A library to do stuff.
module Lib where

import Data.Traversable
import Data.Monoid

-- 1. Identity -------------------------------------------------------------
newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity (f a)

instance Foldable Identity where
    foldMap f (Identity a) = (f a)

instance Traversable Identity where
    traverse f (Identity a) = Identity <$> f a
-------------------------------------------------------------------

-- 2. Constant -------------------------------------------------------------
newtype Constant a b = Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
    pure x = Constant mempty
    Constant _ <*> Constant a = Constant a

instance Monoid a => Foldable (Constant a) where
    foldMap _ (Constant a) = mempty

instance Monoid a => Traversable (Constant a) where
    traverse f (Constant a) = pure $ Constant a
-------------------------------------------------------------------

-- 3. Optional -------------------------------------------------------------
data Optional a = Nada | Yep a deriving (Eq, Show)

instance Functor Optional where
    fmap _ Nada = Nada
    fmap f (Yep a) = Yep (f a)

instance Applicative Optional where
    pure = Yep
    Nada <*> _ = Nada
    _ <*> Nada = Nada
    Yep f <*> Yep a = Yep (f a)

instance Foldable Optional where
    foldMap _ Nada = mempty
    foldMap f (Yep a) = f a

instance Traversable Optional where
    traverse _ Nada = pure $ Nada
    traverse f (Yep a) = Yep <$> (f a)

-- 4. List -----------------------------------------------------------------
data List a = Nil | Cons a (List a)
    deriving (Eq, Show)

appendList :: List a -> List a -> List a
appendList Nil ls = ls
appendList (Cons x ls) ls' = Cons x (appendList ls ls') 

mapList :: (a -> b) -> List a -> List b
mapList _ Nil = Nil
mapList f (Cons x xs) = Cons (f x) (mapList f xs)

flatMapList :: List (a -> b) -> List a -> List b
flatMapList Nil _ = Nil
flatMapList (Cons f fs) ls = mapList f ls `appendList` (flatMapList fs ls) 

foldList :: (a -> b -> b) -> b -> List a -> b
foldList _ acc Nil = acc
foldList f acc (Cons a ls) = foldList f (f a acc) ls  

instance Functor List where
    fmap _ Nil = Nil
    fmap f (Cons a l) = Cons (f a) (fmap f l)

instance Applicative List where
    pure x = Cons x Nil
    fs <*> as = flatMapList fs as 

instance Foldable List where
    foldMap f xs = foldList(\item acc -> (f item) <> acc) mempty xs

instance Traversable List where
    traverse _ Nil = pure $ Nil
    traverse f (Cons x ls) = Cons <$> (f x) <*> traverse f ls 

----------------------------------------------------------------------------

-- 5. Three ----------------------------------------------------------------
data Three a b c = Three a b c deriving (Eq, Show)

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b ( f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    (Three fa fb fc) <*> (Three a b c) = Three (fa <> a) (fb <> b) (fc c)

instance Foldable (Three a b) where
    foldMap f (Three a b c) = f c

instance Traversable (Three a b) where
    traverse f (Three a b c) = (Three a b) <$> (f c)

----------------------------------------------------------------------------

-- 6. S --------------------------------------------------------------------

data S n a = S (n a) a deriving (Eq, Show)

instance Functor n => Functor (S n) where
    fmap f (S n a) = S (fmap f n) (f a)

instance Foldable n => Foldable (S n) where
    foldMap f (S n a) =  foldMap f n <> (f a)

instance Traversable n => Traversable (S n) where
    traverse f (S n a) = S <$> (traverse f n) <*> (f a)  

-- 7. Tree -----------------------------------------------------------------

data Tree a =
    Empty
    | Leaf a
    | Node (Tree a) a (Tree a)
    deriving (Eq, Show)


instance Functor (Tree) where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf (f a)
    fmap f (Node ln a rn) = Node (fmap f ln) (f a) (fmap f rn)

instance Foldable (Tree) where
    foldMap _ Empty = mempty
    foldMap f (Leaf a) = (f a)
    foldMap f (Node ln a rn) = (foldMap f ln) <> (f a) <> (foldMap f rn)

instance Traversable (Tree) where
    traverse _ Empty = pure $ Empty
    traverse f (Leaf a) = Leaf <$> (f a)
    traverse f (Node ln a rn) = Node <$> (traverse f ln) <*> (f a) <*> (traverse f rn)
