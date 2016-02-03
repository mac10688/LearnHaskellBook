-- | A library to do stuff.
module Lib where

import Data.Traversable
import Data.Monoid

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

--instance Traversable List where
traverse f xs = Cons mempty <$> foldMap f xs
