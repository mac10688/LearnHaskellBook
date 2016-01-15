{-# OPTIONS_GHC -fno-warn-orphans #-}

module LibSpec where

import Test.Hspec
import Test.Hspec.QuickCheck
import qualified Test.QuickCheck as Q

import qualified Lib as L

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Lib" $ do
    it "works" $ do
      True `shouldBe` True
    --prop "ourAdd is commutative" $ \x y ->
      --ourAdd x y `shouldBe` ourAdd y x

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

instance (Q.Arbitrary a) => Q.Arbitrary (L.BoolAndSomethingElse a) where
    arbitrary = do
        a <- Q.arbitrary
        Q.elements [L.False' a, L.True' a]

identityBoolAndSomethingElse :: IO ()
identityBoolAndSomethingElse = Q.quickCheck $ \x -> functorIdentity (x :: L.BoolAndSomethingElse Int)

composeBoolAndSomethingElse :: IO ()
composeBoolAndSomethingElse = Q.quickCheck $ \x -> functorCompose (+1) (*2) (x :: L.BoolAndSomethingElse Int)

instance (Q.Arbitrary a) => Q.Arbitrary (L.BoolAndMaybeSomethingElse a) where
    arbitrary = do
        a <- Q.arbitrary
        Q.elements [L.Falsish, L.Truish a]

identityBoolandMaybeSomethingElse :: IO ()
identityBoolandMaybeSomethingElse = Q.quickCheck $ \x -> functorIdentity (x :: L.BoolAndMaybeSomethingElse Int)

composeBoolAndMaybeSomethingElse :: IO ()
composeBoolAndMaybeSomethingElse = Q.quickCheck $ \x -> functorCompose (+1) (*2) (x :: L.BoolAndMaybeSomethingElse Int)

instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (L.Sum a b) where
    arbitrary = do
        a <- Q.arbitrary
        b <- Q.arbitrary
        Q.elements [L.First a, L.Second b]

identitySum :: IO ()
identitySum = Q.quickCheck $ \x -> functorIdentity (x :: L.Sum Int Int)

composeSum :: IO ()
composeSum = Q.quickCheck $ \x -> functorCompose (+1) (*2) (x :: L.Sum Int Int)

instance (Q.Arbitrary a, Q.Arbitrary b, Q.Arbitrary c) => Q.Arbitrary (L.Company a b c) where
    arbitrary = do
        a <- Q.arbitrary
        b <- Q.arbitrary
        c <- Q.arbitrary
        Q.elements [L.DeepBlue a c, L.Something b]

identityCompany :: IO ()
identityCompany = Q.quickCheck $ \x -> functorIdentity (x :: L.Company Int Int Int)

composeCompany :: IO ()
composeCompany = Q.quickCheck $ \x -> functorCompose (+1) (*2) (x :: L.Company Int Int Int)

instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (L.More b a) where
    arbitrary = do
        a <- Q.arbitrary
        a' <- Q.arbitrary
        b <- Q.arbitrary
        b' <- Q.arbitrary
        Q.elements [L.L a b a', L.R b a b']

identityMore :: IO ()
identityMore = Q.quickCheck $ \x -> functorIdentity (x :: L.More Int Int)

composeMore :: IO ()
composeMore = Q.quickCheck $ \x -> functorCompose (+1) (*2) (x :: L.More Int Int)

instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (L.Quant a b) where
    arbitrary = do
        a <- Q.arbitrary
        b <- Q.arbitrary
        Q.elements[L.Finance, L.Desk a, L.Bloor b]

identityQuant :: IO ()
identityQuant = Q.quickCheck $ \x -> functorIdentity (x :: L.Quant Int Int)

composeQuant :: IO ()
composeQuant = Q.quickCheck $ \x -> functorCompose (+1) (*2) (x :: L.Quant Int Int)

instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (L.K a b) where
    arbitrary = do
        a <- Q.arbitrary
        Q.elements [L.K a]

identityK :: IO ()
identityK = Q.quickCheck $ \x -> functorIdentity (x :: L.K Int Int)

composeK :: IO ()
composeK = Q.quickCheck $ \x -> functorCompose (+1) (*2) (x :: L.K Int Int)

--identityL' :: IO ()
--identityL' = Q.quickCheck $ \x -> functorIdentity (x :: 
