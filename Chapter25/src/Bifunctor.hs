module Bifunctor where

class Bifunctor p where
    {-# MINIMAL bimap | first, second #-}

    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
        bimap f s (Deux a b) = Deux (f a) (s b)

data Const a b = Const a

instance Bifunctor Const where
        bimap f s (Const a) = Const (f a)

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    bimap f s (Drei a b c) = Drei a (f b) (s c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap f s (SuperDrei a b) = SuperDrei a (f b) 

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap f s (SemiDrei a) = SemiDrei a

data Quadriceps a b c d = Quadzzz a b c d

instance Bifunctor (Quadriceps a b) where
    bimap f s (Quadzzz a b c d) = Quadzzz a b (f c) (s d)

data Either a b = Left a | Right b

instance Bifunctor Bifunctor.Either where
    bimap f s (Bifunctor.Left a) = Bifunctor.Left (f a)
    bimap f s (Bifunctor.Right b) = Bifunctor.Right (s b)
