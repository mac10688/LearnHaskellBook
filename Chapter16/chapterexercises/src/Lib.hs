{-# LANGUAGE FlexibleInstances #-}
-- | A library to do stuff.
module Lib where

--Determine if a valid Functor can be written for the datatype provided.
import GHC.Arr

--1.
data Bool = False | True
--No, because it has kind *

--2.
data BoolAndSomethingElse a = False' a | True' a
    deriving (Eq, Show)
--Yes, because it has kind * -> *

instance Functor BoolAndSomethingElse where
    fmap f (False' a) = False' (f a)
    fmap f (True' a) = True' (f a)

--3.
data BoolAndMaybeSomethingElse a = Falsish | Truish a
    deriving (Eq, Show)
--Yes, because it has kind * -> *

instance Functor BoolAndMaybeSomethingElse where
    fmap _ Falsish = Falsish
    fmap f (Truish a) = Truish (f a)

--4. Use the kinds to guide you on this one, don't get too hung up on the details
newtype Mu f = InF { outF :: f (Mu f) }

--instance Functor Mu where
--    fmap 

--5. Again, just follow the kinds and ignore the unfamiliar parts
--import GHC.Arr

data D = D (Array Word Word) Int Int
--No, only one kind

--Rearrange the arguments to the type constructor of the datatype so the Functor instance works.
--1.
data Sum a b = First a | Second b
    deriving (Eq, Show)

instance Functor (Sum e) where
    fmap f (First a) = First a
    fmap f (Second b) = Second (f b)

--2.
data Company a b c = DeepBlue a c | Something b
    deriving (Eq, Show)

instance Functor (Company e e') where
    fmap _ (Something b) = Something b
    fmap f (DeepBlue a c) = DeepBlue a (f c)

--3.

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More b) where
    fmap f (L a b a') = L (f a) b (f a')
    fmap f (R b a b') = R b (f a) b'


--Prelude> fmap (+1) (L 1 2 3)
--L 2 2 4
--Prelude> fmap (+1) (R 1 2 3)
--R 1 3 3

--Write Functor instances for the following datatypes.
--1.
data Quant a b = Finance | Desk a | Bloor b deriving (Eq, Show)

instance Functor (Quant a) where
    fmap _ Finance = Finance
    fmap f (Desk a) = Desk a
    fmap f (Bloor b) = Bloor (f b)

--2. No, it's not interesting by itself.
data K a b = K a deriving (Eq, Show)

instance Functor (K a) where
    fmap f (K a) = K a

--3. {-# LANGUAGE FlexibleInstances #-}
--newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

newtype L' a b = L' a

newtype Flip f a b =
    Flip (f b a)
    deriving (Eq, Show)

-- should remind you of an instance you've written before
instance Functor (Flip L' a) where
    fmap f (Flip (L' a)) = Flip $ L' (f a)

--4. No, it doesn't do anything interesting. No magic here or in the previous exercise. If it works, you succeeded.
data EvilGoateeConst a b = GoatyConst b

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst (f b)

--5. Do you need something extra to make the instance work?
data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f' (LiftItOut fa) = LiftItOut (fmap f' fa)

--6.
data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f' (DaWrappa fa ga) = DaWrappa (fmap f' fa) (fmap f' ga)

--7. Don't ask for more typeclass instances than you need. You can let GHC tell you what to do.
data IgnoreOne f g a b = IgnoringSomething (f a) (g b)

instance (Functor g) => Functor (IgnoreOne f g a) where
    fmap f (IgnoringSomething fa gb) = IgnoringSomething fa (fmap f gb)

--8.
data Notorious g o a t = Notorious (g o) (g a) (g t)

instance (Functor g) => Functor (Notorious g o a) where
    fmap f (Notorious go ga gt) = Notorious go ga (fmap f gt)

--9. You'll need to use recursion.
data List a = Nil | Cons a (List a)

instance Functor List where
    fmap f (Nil) = Nil
    fmap f (Cons a la) = Cons (f a) (fmap f la) 

--10.
data GoatLord a = NoGoat | OneGoat a | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a)

instance Functor GoatLord where
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat (f a)
    fmap f (MoreGoats fa fb fc) = MoreGoats (fmap f fa) (fmap f fb) (fmap f fc)

--11.
data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt
    fmap f (Print x a) = Print x (f a)
    fmap f (Read fa) = Read (fmap f fa)
