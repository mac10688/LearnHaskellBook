{-# OPTIONS_GHC -fno-warn-orphans #-}

module MainTests where

import Test.Hspec
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import Data.Monoid

import Lib 

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = do
        a <- arbitrary
        return $ Identity a

instance (Eq a) => EqProp (Identity a) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Sum a) where
    arbitrary = do
        a <- arbitrary
        return $ Sum a

instance Arbitrary a => Arbitrary (Constant a b) where
    arbitrary = do
        a <- arbitrary
        return $ Constant a

instance Eq a => EqProp (Constant a b) where
    (=-=) = eq

instance Arbitrary a => Arbitrary (Optional a) where
    arbitrary = do
        a <- arbitrary
        elements [Nada, (Yep a)]

instance Eq a => EqProp (Optional a) where
    (=-=) = eq

main = do
    let identity = undefined :: Identity (Int, Int, [Int])
    quickBatch (traversable identity)
    let constant = undefined :: Constant (Sum Int) (Int, Int, [Int])
    quickBatch (traversable constant)
    let optional = undefined :: Optional (Int, Int, [Int])
    quickBatch (traversable optional)
