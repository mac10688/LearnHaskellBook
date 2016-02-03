-- | A library to do stuff.
module Lib where

import Data.Monoid

--1. This and the next one are nicer with foldMap, but foldr is fine too
sum' :: (Foldable t, Num a) => t a -> a
sum' ta = getSum $ foldMap Sum ta

--2.
product' :: (Foldable t, Num a) => t a -> a
product' ta = getProduct $ foldMap Product ta

--3.
elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' x = foldr (\item acc -> (item == x) || acc) False

--4.
minimum' :: (Foldable t, Ord a) => t a -> a
minimum' = foldMap 

--5.
maximum' :: (Foldable t, Ord a) => t a -> a
maximum' = undefined

--6.
null' :: (Foldable t) => t a -> Bool
null' = foldr (\x y -> False) True

--7.
length' :: (Foldable t) => t a -> Int
length' = foldr (\ _ y -> y+1 ) 0

--8. Some say this is all foldable amounts to.
toList' :: (Foldable t) => t a -> [a]
toList' = foldr (\x y -> x:y) []

--9. Hint: use foldMap
-- / Combine the elements of a structure using a monoid.
fold' :: (Foldable t, Monoid m) => t m -> m
fold' = foldMap (\x -> x <> mempty) 

--10. Define foldMap in terms of foldr
foldMap' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap' f = foldr(\item acc -> (f item) <> acc ) mempty

--Write Foldable instances for the following datatypes

--1.
data Constant a b = Constant a

instance Foldable (Constant a) where
    foldMap _ (Constant a) = mempty 

--2.
data Two a b = Two a b

instance Foldable (Two a) where
    foldMap f (Two a b) = (f b)

--3.
data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldMap f (Three a b c) = (f c)

--4.
data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' a b c) = (f b) <> (f c)

--5.
data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' a b c d) = (f b) <> (f c) <> (f d)

--Thinking cap time. Write a filter function for Foldable types using foldMap

filterF :: (Applicative f, Foldable f, Monoid (f a)) => (a -> Bool) -> f a -> f a
filterF f = foldMap (\x -> if (f x) then pure x else mempty) 
