{-# OPTIONS_GHC -fno-warn-orphans #-}

module LibSpec where

import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid

import Lib 

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
        a <- arbitrary
        return (Sum a)

instance Arbitrary a => Arbitrary (Nope a) where
    arbitrary = return NopeDotJpg

instance EqProp (Nope a) where (=-=) = eq

instance (Arbitrary a, Arbitrary b) => Arbitrary (PhbtEither a b) where
    arbitrary = do
        a <- arbitrary
        b <- arbitrary
        elements [Lib.Left b, Lib.Left a]

instance (Eq a, Eq b) => EqProp (PhbtEither a b) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance (Eq a) => EqProp (Identity a) where (=-=) = eq

instance (Arbitrary a) => Arbitrary (List a) where
    arbitrary = do
        a <- arbitrary
        return (Cons a Nil)

instance (Eq a) => EqProp (List a) where (=-=) = eq

main = do
    let nope = undefined :: Nope (Int, Int, Int)
    quickBatch $ functor nope
    quickBatch $ applicative nope
    quickBatch $ monad nope
    let phbtEither = undefined :: PhbtEither (Sum Int) (Int, Int, Int)
    quickBatch $ functor phbtEither
    quickBatch $ applicative phbtEither
    quickBatch $ monad phbtEither 
    let identity = undefined :: Identity (Int, Int, Int)
    quickBatch $ functor identity
    quickBatch $ applicative identity
    quickBatch $ monad identity 
    let list = undefined :: List (Int, Int, Int)
    quickBatch $ functor list
    quickBatch $ applicative list
    quickBatch $ monad list
