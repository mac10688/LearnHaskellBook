{-# OPTIONS_GHC -fno-warn-orphans #-}

module LibSpec where

import Test.Hspec
import Test.QuickCheck

import Data.Monoid

import Lib 


newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Ord, Show)

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance (Monoid a) => Applicative (Constant a) where
    pure x = Constant (mempty x )
    _ <*> (Constant a) = Constant a

