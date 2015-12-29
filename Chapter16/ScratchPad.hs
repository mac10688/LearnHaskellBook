{-# LANGUAGE FlexibleInstances #-}

module FlipFunctor where

data Tuple a b =
    Tuple a b
    deriving (Eq, Show)

-- this is impossible in Haskell
--instance Functor (Tuple ? b) where
--    fmap f (Tuple a b) = Tuple (f a) b

newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

-- this actually works, goofy as it looks.
instance Functor (Flip Tuple a) where
    fmap f (Flip (Tuple a b)) = Flip $ Tuple (f a) b
