{-# OPTIONS_GHC -fno-warn-orphans #-}

module LibSpec where

import Data.Monoid
import Control.Applicative (liftA3)

--import Test.Hspec
--import Test.QuickCheck

--import Lib 

--1. Type
--[]
--
--Methods
--pure :: a -> [a]
--(<*>) :: [(a -> b)] -> [a] -> [b]
--
--2. Type 
--()
--
--Methods
--pure :: a -> IO a
--(<*>) :: IO (a -> b) -> IO a -> IO b
--
--3. Type
--(,) a
--
--Methods
--pure :: (Monoid a) => b -> (a, b)
--(<*>) :: (Monoid m => (m,(a -> b)) -> (m,a) -> (m,b)
--
--4. Type
--(->) e
--
--Methods
--pure :: b -> (a -> b)
--(<*>) :: (a -> b -> c) -> (a -> b) -> (a -> c)

newtype Identity a = Identity a deriving Show

instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

instance Applicative Identity where
    pure = Identity
    Identity f <*> Identity a = Identity (f a) 

data Pair a = Pair a a deriving Show

instance Functor Pair where
    fmap f (Pair a a') = Pair (f a) (f a')

instance Applicative Pair where
    pure x = Pair x x
    Pair f f' <*> Pair x x' = Pair (f x) (f' x')

data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    Two a f <*> Two a' b = Two ( a <> a') (f b)

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    Three a b f <*> Three a' b' c' = Three (a <> a') (b <> b') (f c')

data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x
    (Three' a f f') <*> (Three' a' b b') = Three' (a <> a') (f b) (f' b')

data Four a b c d = Four a b c d

instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

instance (Monoid a, Monoid b, Monoid c) => Applicative (Four a b c) where
    pure x = Four mempty mempty mempty x
    (Four a b c f) <*> (Four a' b' c' d) = Four (a <> a') (b <> b') (c <> c') (f d)

data Four' a b = Four' a a a b

instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a b c (f d)

instance (Monoid a) => Applicative (Four' a) where
    pure x = Four' mempty mempty mempty x
    (Four' a b c f) <*> (Four' a' b' c' d) = Four' (a <> a') (b <> b') (c <> c') (f d)

--1. Given the following sets of consonants and vowels:
--stops = "pbtdkg"
--vowels = "aeiou"
--a) Write a function that takes inputs from stops and vowels and
--makes 3-tuples of all possible stop-vowel-stop combinations. These
--will not all correspond to real words in English, although the
--stop-vowel-stop pattern is common enough that many of them
--will.
--b) Modify that function so that it only returns the combinations
--that begin with a p.
--c) Now set up lists of nouns and verbs (instead of stops and vowels)
--and modify the function to make tuples representing possible
--noun-verb-noun sentences.

stops, vowels :: String
stops = "pbtdkg"
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)]
combos = liftA3 (\a b c -> (a, b, c))
