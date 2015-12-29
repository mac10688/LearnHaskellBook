{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RankNTypes #-}

import Test.QuickCheck
import Test.QuickCheck.Function

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

functorCompose' :: (Eq (f c), Functor f) => f a -> Fun a b -> Fun b c -> Bool
functorCompose' x (Fun _ f) (Fun _ g) = (fmap (g . f) x) == (fmap g . fmap f $ x)

newtype Identity a = Identity a
instance Functor Identity where
    fmap f (Identity a) = Identity (f a)

data Pair a = Pair a a
instance Functor Pair where
    fmap f (Pair a b) = Pair (f a) (f b)

data Two a b = Two a b
instance Functor (Two a) where
    fmap f (Two a b) = Two a (f b)

data Three a b c = Three a b c
instance Functor (Three a b) where
    fmap f (Three a b c) = Three a b (f c)

data Three' a b = Three' a b b
instance Functor (Three' a) where
    fmap f (Three' a b c) = Three' a (f b) (f c)

data Four a b c d = Four a b c d
instance Functor (Four a b c) where
    fmap f (Four a b c d) = Four a b c (f d)

data Four' a b = Four' a a a b
instance Functor (Four' a) where
    fmap f (Four' a b c d) = Four' a  b c (f d)

data Possibly a = LolNope | Yeppers a deriving (Eq, Show)

instance Functor Possibly where
    fmap _ LolNope = LolNope
    fmap f (Yeppers a) = Yeppers (f a)

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

newtype Constant a b =
    Constant { getConstant :: a }
    deriving (Eq, Show)

instance Functor (Constant m) where
    fmap _ (Constant v) = Constant v

data Wrap f a =
    Wrap (f a)
    deriving (Eq, Show)

instance Functor f => Functor (Wrap f) where
    fmap f' (Wrap fa) = Wrap (fmap f' fa)

getInt :: IO Int
getInt = fmap read getLine

meTooIsm :: IO String
meTooIsm = do
    input <- getLine
    return (input ++ "and me too!")

bumpIt :: IO Int
bumpIt = do
    intVal <- getInt
    return (intVal + 1)

type Nat f g = forall a. f a -> g a

main :: IO ()
main = putStrLn "Test suite not yet implemented"
