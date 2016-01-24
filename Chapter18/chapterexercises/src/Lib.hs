-- | A library to do stuff.
module Lib where

import Data.Monoid ((<>))
import Control.Monad

data Nope a = NopeDotJpg deriving (Eq, Show)

instance Functor Nope where
    fmap _ _ = NopeDotJpg

instance Applicative Nope where
    pure _ = NopeDotJpg
    _ <*> _ = NopeDotJpg

instance Monad Nope where
    return = pure
    _ >>= _ = NopeDotJpg

data PhbtEither b a =
      Left a
    | Right b
    deriving (Eq, Show)

instance Functor (PhbtEither b) where
    fmap f (Lib.Left a) = Lib.Left (f a)
    fmap _ (Lib.Right b) = Lib.Right b

instance Monoid b => Applicative (PhbtEither b) where
    pure = Lib.Left
    (Lib.Left f) <*> (Lib.Left a) = Lib.Left (f a)
    (Lib.Right f) <*> (Lib.Right a) = Lib.Right (f <> a)
    (Lib.Left _) <*> (Lib.Right a) = Lib.Right a
    (Lib.Right f) <*> (Lib.Left _) = Lib.Right f

instance Monoid b => Monad (PhbtEither b) where
    return = pure
    (Lib.Left a) >>= f = (f a)
    (Lib.Right a) >>=  _ = Lib.Right a

newtype Identity a = Identity a
    deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    (Identity f) <*> (Identity a) = Identity (f a)

instance Monad Identity where
    return = pure
    (Identity a) >>= f = (f a)

data List a = Nil | Cons a (List a) deriving (Eq, Show)

take :: Int -> List x -> [x]
take _ Nil = []
take n (Cons x ls)
    | n > 0 = x : Lib.take (n-1) ls
    | otherwise = []

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ f <$> xs

repeatApply :: List (a -> b) -> a -> List b
repeatApply Nil _ = Nil
repeatApply (Cons f l) a = Cons (f a) (repeatApply l a)

instance Functor List where
    fmap f (Cons a l) = Cons (f a) (fmap f l)
    fmap _ Nil = Nil

instance Applicative List where
    pure x = Cons x Nil
    Nil <*> _ = Nil
    _ <*> Nil = Nil
    fs <*> ls = flatMap (repeatApply fs) ls

instance Monad List where
    return = pure
    Nil >>= _ = Nil
    xs >>= f = flatMap f xs

j :: Monad m => m (m a) -> m a
j = join

l1 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
l1 = liftM2

d :: Monad m => m a -> m (a -> b) -> m b
d = flip $ ap

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh = forM 

flipType :: (Monad m) => [m a] -> m [a]
flipType = sequence

