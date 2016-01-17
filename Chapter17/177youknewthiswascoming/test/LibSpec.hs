{-# OPTIONS_GHC -fno-warn-orphans #-}
module LibSpec where

import Control.Applicative
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Lib 

-- this isn't going to work properly
--instance Monoid a => Monoid (ZipList a) where
 --   mempty = ZipList []
  --  mappend = liftA2 mappend

instance Monoid a => Monoid (ZipList a) where
    mempty = pure mempty
    mappend = liftA2 mappend

instance Arbitrary Bull where
    arbitrary =
        frequency [ (1, return Fools)
                  , (1, return Twoo) ]

data List a =
        Nil
      | Cons a (List a)
      deriving (Eq, Show)


take :: Int -> List x -> [x]
take _ Nil = []
take n (Cons x ls)
     | n > 0 = x : LibSpec.take (n-1) ls  
     | otherwise = []

append :: List a -> List a -> List a
append Nil ys = ys
append (Cons x xs) ys = Cons x $ append xs ys

fold :: (a -> b -> b) -> b -> List a -> b
fold _ b Nil = b
fold f b (Cons h t) = f h (fold f b t)

concat' :: List (List a) -> List a
concat' = fold append Nil

instance Functor List where
    fmap f (Cons x l) = Cons (f x) (fmap f l)
    fmap _ Nil = Nil

-- write this one in terms of concat' and fmap
flatMap :: (a -> List b) -> List a -> List b
flatMap f xs = concat' $ f <$> xs

checkIt :: List (a -> b) -> a -> List b
checkIt Nil _ = Nil
checkIt (Cons f l) a = Cons (f a) (checkIt l a) 

instance Applicative List where
    pure x = (Cons x) Nil
    _ <*> Nil = Nil
    Nil <*> _ = Nil
    fs <*> ls = flatMap (checkIt fs) ls 

functions :: List (Int -> Int)
functions = Cons (+1) (Cons (*2) Nil)

values :: List Int 
values = Cons 1 (Cons (2 :: Int) Nil)

test :: List Int
test = functions <*> values

newtype ZipList' a =
    ZipList' (List a)
    deriving (Eq, Show)

instance Eq a => EqProp (ZipList' a) where
    xs =-= ys = xs' `eq` ys'
        where xs' = let (ZipList' l) = xs
                    in LibSpec.take 3000 l
              ys' = let (ZipList' l) = ys
                    in LibSpec.take 3000 l

instance Functor ZipList' where
    fmap f (ZipList' xs) = ZipList' $ fmap f xs

instance Applicative ZipList' where
    pure x = ZipList' (pure x)
    ZipList' xs <*> ZipList' ys = ZipList' (xs <*> ys)

instance EqProp Bull where (=-=) = eq

instance Arbitrary a => Arbitrary (ZipList a) where
    arbitrary = ZipList <$> arbitrary

instance Arbitrary a => Arbitrary (Data.Monoid.Sum a) where
    arbitrary = Data.Monoid.Sum <$> arbitrary

instance Eq a => EqProp (ZipList a) where (=-=) = eq

data Sum a b =
      First a
    | Second b
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (LibSpec.Sum a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (LibSpec.First a))
                  ,(1, return (LibSpec.Second b))]

data Validation e a =
      Error e
    | Success a
    deriving (Eq, Show)

instance (Arbitrary a, Arbitrary b) => Arbitrary (LibSpec.Validation a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        frequency [(1, return (LibSpec.Error a))
                  ,(1, return (LibSpec.Success b))]

instance Functor (LibSpec.Sum a) where
    fmap f (LibSpec.Second b) = LibSpec.Second (f b)
    fmap _ (LibSpec.First a) = LibSpec.First a

instance Applicative (LibSpec.Sum a) where
    pure = Second 
    LibSpec.Second f <*> LibSpec.Second a = LibSpec.Second $ (f a)
    LibSpec.Second _ <*> LibSpec.First a = LibSpec.First a
    LibSpec.First a <*> _ = LibSpec.First a

instance Functor (Validation e) where
    fmap f (LibSpec.Success a) = LibSpec.Success $ (f a)
    fmap _ (LibSpec.Error a) = LibSpec.Error a

instance Monoid e => Applicative (Validation e) where
    pure = LibSpec.Success
    LibSpec.Success f <*> LibSpec.Success a = LibSpec.Success ( f a)
    LibSpec.Success _ <*> LibSpec.Error a = LibSpec.Error a
    LibSpec.Error a <*> LibSpec.Success _ = LibSpec.Error a
    LibSpec.Error a <*> LibSpec.Error a' = LibSpec.Error (a <> a')

instance (CoArbitrary a) => CoArbitrary (Data.Monoid.Sum a) where
    coarbitrary (Sum x) = coarbitrary x

instance (Eq a, Eq b) => EqProp (LibSpec.Sum a b) where
   (=-=) = eq
   
instance (Eq a, Eq b) => EqProp (LibSpec.Validation a b) where
    (=-=) = eq

main :: IO ()
main = quickBatch (monoid Twoo)
