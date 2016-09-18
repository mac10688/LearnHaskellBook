{-# LANGUAGE InstanceSigs  #-}

module Compose where

newtype Compose f g a =
    Compose { getCompose :: f (g a)}
    deriving (Eq, Show)

instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure :: a -> Compose f g a
    pure a = Compose $ (pure . pure) a

    (<*>) :: Compose f g (a -> b) -> Compose f g a -> Compose f g b
    (Compose fga) <*> (Compose fga') 
        = Compose $ (<*>) <$> fga <*> fga'

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap :: (Monoid m) => (a -> m) -> Compose f g a -> m 
    foldMap f (Compose fg) = (foldMap . foldMap) f fg

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse :: (Applicative app) => (a -> app b) -> Compose f g a -> app (Compose f g b)
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga


