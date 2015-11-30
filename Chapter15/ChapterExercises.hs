module ChapterExercises where

import Data.Semigroup
import qualified Test.QuickCheck as Q
import qualified Data.Monoid as M
import Data.Word

data Trivial = Trivial
  deriving (Eq, Show)

instance Semigroup Trivial where
   (<>) = trivialMush

instance M.Monoid Trivial where
  mempty = Trivial
  mappend = (<>)

instance Q.Arbitrary Trivial where
  arbitrary = return Trivial

trivialMush :: Trivial -> Trivial -> Trivial
trivialMush _ _ = Trivial

prop_Associative :: (Semigroup a, Eq a) => a -> a -> a -> Bool
prop_Associative a b c = a <> (b <> c) == (a <> b) <> c

prop_LeftIdentity :: (Eq a, Monoid a) => a -> Bool
prop_LeftIdentity a = (mempty M.<> a) == a

prop_RightIdentity :: (Eq a, Monoid a) => a -> Bool
prop_RightIdentity a = (a M.<> mempty) == a

newtype Identity a = Identity a
  deriving (Eq, Show)

instance (Semigroup a) => Semigroup (Identity a) where
  Identity a <> Identity b = Identity (a <> b)

instance (M.Monoid a) => M.Monoid (Identity a) where
  mempty = Identity M.mempty
  Identity a `mappend` Identity a' = Identity (a M.<> a')

arbitraryIdentity :: (Q.Arbitrary a) => Q.Gen (Identity a)
arbitraryIdentity = do
  x <- Q.arbitrary
  return (Identity x)

instance (Q.Arbitrary a) => Q.Arbitrary (Identity a) where
  arbitrary = arbitraryIdentity

data Two a b = Two a b
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  Two a b <> Two a' b' = Two (a <> a') (b <> b')

instance (M.Monoid a, M.Monoid b) => M.Monoid (Two a b) where
  mempty = Two M.mempty M.mempty
  Two a b `mappend` Two a' b' = Two (a M.<> a') (b M.<> b')

instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (Two a b) where
  arbitrary = do
    x <- Q.arbitrary
    y <- Q.arbitrary
    return $ Two x y

data Three a b c = Three a b c
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c) => Semigroup (Three a b c) where
  Three a b c <> Three a' b' c' = Three (a <> a') (b <> b') (c <> c')

instance (Monoid a, Monoid b, Monoid c) => Monoid (Three a b c) where
  mempty = Three M.mempty M.mempty M.mempty
  Three a b c `mappend` Three a' b' c' = Three (a M.<> a') (b M.<> b') (c M.<> c')

instance (Q.Arbitrary a, Q.Arbitrary b, Q.Arbitrary c) => Q.Arbitrary (Three a b c) where
  arbitrary = do
    x <- Q.arbitrary
    y <- Q.arbitrary
    z <- Q.arbitrary
    return $ Three x y z

data Four a b c d = Four a b c d
  deriving (Eq, Show)

instance (Semigroup a, Semigroup b, Semigroup c, Semigroup d) => Semigroup (Four a b c d) where
  Four a b c d <> Four a' b' c' d' = Four (a<>a') (b <> b') (c <> c') (d <> d')

instance (Monoid a, Monoid b, Monoid c, Monoid d) => Monoid (Four a b c d) where
  mempty = Four mempty mempty mempty mempty
  Four a b c d `mappend` Four a' b' c' d' = Four (a M.<> a') (b M.<> b') (c M.<> c') (d M.<> d')

instance (Q.Arbitrary a, Q.Arbitrary b, Q.Arbitrary c, Q.Arbitrary d) => Q.Arbitrary (Four a b c d) where
  arbitrary = do
    x <- Q.arbitrary
    y <- Q.arbitrary
    z <- Q.arbitrary
    z' <- Q.arbitrary
    return $ Four x y z z'

newtype BoolConj = BoolConj Bool
  deriving (Eq, Show)

instance Semigroup (BoolConj) where
  BoolConj True <> BoolConj True = BoolConj True
  _ <> _ = BoolConj False

instance Monoid (BoolConj) where
  mempty = BoolConj True
  a `mappend` b = a <> b

instance Q.Arbitrary BoolConj where
  arbitrary = Q.frequency [(1, return $ BoolConj True)
                        , (1, return $ BoolConj False)]

newtype BoolDisj = BoolDisj Bool
  deriving (Eq, Show)

instance Semigroup (BoolDisj) where
  BoolDisj True <> _ = BoolDisj True
  _ <> BoolDisj True = BoolDisj True
  _ <> _ = BoolDisj False

instance Monoid (BoolDisj) where
  mempty = BoolDisj False
  a `mappend` b = a <> b

instance Q.Arbitrary BoolDisj where
  arbitrary = Q.frequency [(1, return $ BoolDisj True)
                        , (1, return $ BoolDisj False)]

data Or a b = Fst a | Snd b
  deriving (Eq, Show)

instance Semigroup (Or a b) where
  Snd a <> _ = Snd a
  _ <> Snd a = Snd a
  Fst a <> Fst _ = Fst a

instance (Monoid a) =>  Monoid (Or a b) where
  mempty = Fst mempty
  Snd a `mappend` _ = Snd a
  _ `mappend` Snd a = Snd a
  Fst a `mappend` Fst a' = Fst ( a M.<> a')


instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (Or a b) where
  arbitrary = do
    a <- Q.arbitrary
    b <- Q.arbitrary
    Q.frequency [(1, return $ Fst a)
              ,(1, return $ Snd b)]

newtype Combine a b =
  Combine { unCombine :: a -> b }

instance Semigroup b => Semigroup (Combine a b) where
  x <> y = Combine $ \n -> unCombine x n <> unCombine y n

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine $ \_ -> mempty
  x `mappend` y = Combine $ \n -> unCombine x n `mappend` unCombine y n

instance Show (a -> b) where
  show _ = "Test function"

prop_CombineAssociative :: (Eq b, Show b, Monoid b) => (a -> b) -> (a -> b) -> (a -> b) -> a -> Bool
prop_CombineAssociative a b c d = let
                        a' = Combine a
                        b' = Combine b
                        c' = Combine c
                        in unCombine ((a' M.<> b') M.<> c') d == unCombine (a' M.<> (b' M.<> c')) d

prop_CombineLeftIdentity :: (Eq b, Show b, Monoid b) => (a -> b) -> a -> Bool
prop_CombineLeftIdentity f a =
  let f' = Combine f in unCombine (f' M.<> mempty) a == f a

prop_CombineRightIdentity :: (Eq b, Show b, Monoid b) => (a -> b) -> a -> Bool
prop_CombineRightIdentity f a =
  let f' = Combine f in unCombine (mempty M.<> f') a == f a

instance (Q.Arbitrary a) => Q.Arbitrary (Sum a) where
  arbitrary = do
    x <- Q.arbitrary
    return $ Sum x

-- instance  Q.CoArbitrary (Sum a) where
  -- coarbitrary = Q.coarbitraryShow

newtype Comp a = Comp {unComp :: a -> a}

instance Semigroup (Comp a) where
  x <> y = let
   x' = unComp x
   y' = unComp y
   in Comp $ \n -> y' $ x' n

instance Monoid a => Monoid (Comp a) where
  mempty = Comp $ \_ -> mempty
  x `mappend` y = let
      x' = unComp x
      y' = unComp y
      in Comp $ \n -> (x' n) M.<> (y' n)

prop_CompAssociative :: (Eq a, Monoid a) => (a -> a) -> (a -> a) -> (a -> a) -> a -> Bool
prop_CompAssociative a b c d = let
  a' = Comp a
  b' = Comp b
  c' = Comp c
  in unComp ((a' M.<> b') M.<> c') d == unComp (a' M.<> (b' M.<> c')) d

prop_CompLeftIdentity :: (Eq a, Monoid a) => (a -> a) -> a -> Bool
prop_CompLeftIdentity f a =
  let f' = Comp f in unComp (mempty M.<> f') a == f a

prop_CompRightIdentity :: (Eq a, Monoid a) => (a -> a) -> a -> Bool
prop_CompRightIdentity f a =
  let f' = Comp f in unComp (mempty M.<> f') a == f a

data Validation a b =
  Failure a | Success b
  deriving (Eq, Show)

instance Semigroup a => Semigroup (Validation a b) where
  Failure a <> Success _ = Failure a
  Success _ <> Failure a = Failure a
  Failure a <> Failure b = Failure (a <> b)
  Success _ <> Success b = Success b

instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (Validation a b) where
  arbitrary = do
    a <- Q.arbitrary
    b <- Q.arbitrary
    Q.frequency [(1, return $ Failure a), (1, return $ Success b)]

newtype AccumulateRight a b =
  AccumulateRight (Validation a b)
  deriving (Eq, Show)

instance Semigroup b => Semigroup (AccumulateRight a b) where
  AccumulateRight (Success a) <> AccumulateRight (Success b) = AccumulateRight $ Success (a<>b)
  AccumulateRight (Success a) <> _ = AccumulateRight $ Success a
  _ <> AccumulateRight (Success a) = AccumulateRight $ Success a
  AccumulateRight (Failure a) <> _ = AccumulateRight $ Failure a

instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (AccumulateRight a b) where
  arbitrary = do
    a <- Q.arbitrary
    b <- Q.arbitrary
    Q.frequency [(1, return $ AccumulateRight (Failure a)), (1, return $ AccumulateRight (Success b))]

newtype AccumulateBoth a b = AccumulateBoth (Validation a b) deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (AccumulateBoth a b) where
  AccumulateBoth (Failure a) <> AccumulateBoth (Failure b) = AccumulateBoth (Failure (a<>b))
  AccumulateBoth (Success a) <> AccumulateBoth (Success b) = AccumulateBoth (Success (a<>b))
  AccumulateBoth (Failure a) <> _ = AccumulateBoth (Failure a)
  _ <> AccumulateBoth (Failure a) = AccumulateBoth (Failure a)

instance (Q.Arbitrary a, Q.Arbitrary b) => Q.Arbitrary (AccumulateBoth a b) where
  arbitrary = do
    a <- Q.arbitrary
    b <- Q.arbitrary
    Q.frequency [(1, return $ AccumulateBoth (Failure a)), (1, return $ AccumulateBoth (Success b))]

newtype Observe r a = Observe { runObserve :: r -> a }

instance Monoid a => Monoid (Observe r a) where
  mempty = Observe $ \_ -> mempty
  x `mappend` y = Observe $ \n -> (runObserve x n) M.<> (runObserve y n)

newtype Mem s a = Mem { runMem :: s -> (s, a) }

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (s, mempty)
  x `mappend` y = Mem $ \s ->
                let
                  (s',a) = runMem x s
                  (s'', a') = runMem y s'
                in (s'', a M.<> a')

main :: IO ()
main = do
  Q.quickCheck (prop_Associative :: Trivial -> Trivial -> Trivial -> Bool)
  Q.quickCheck (prop_LeftIdentity :: Trivial -> Bool)
  Q.quickCheck (prop_RightIdentity :: Trivial -> Bool)
  Q.quickCheck (prop_Associative :: Identity (Sum Int) -> Identity (Sum Int) -> Identity (Sum Int) -> Bool)
  Q.quickCheck (prop_LeftIdentity :: Identity (Sum Int) -> Bool)
  Q.quickCheck (prop_RightIdentity :: Identity (Sum Int) -> Bool)
  Q.quickCheck (prop_Associative :: Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Two (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_LeftIdentity:: Two (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_RightIdentity:: Two (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_Associative :: Three (Sum Int) (Sum Int) (Sum Int) -> Three (Sum Int) (Sum Int) (Sum Int) -> Three (Sum Int) (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_Associative :: Four (Sum Int) (Sum Int) (Sum Int) (Sum Int) -> Four (Sum Int) (Sum Int) (Sum Int) (Sum Int) -> Four (Sum Int) (Sum Int) (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_LeftIdentity :: Four (Sum Int) (Sum Int) (Sum Int) (Sum Int)-> Bool)
  Q.quickCheck (prop_RightIdentity :: Four (Sum Int) (Sum Int) (Sum Int) (Sum Int)-> Bool)
  Q.quickCheck (prop_Associative :: BoolConj -> BoolConj -> BoolConj -> Bool)
  Q.quickCheck (prop_LeftIdentity :: BoolConj -> Bool)
  Q.quickCheck (prop_RightIdentity :: BoolConj -> Bool)
  Q.quickCheck (prop_Associative :: BoolDisj -> BoolDisj -> BoolDisj -> Bool)
  Q.quickCheck (prop_LeftIdentity :: BoolDisj -> Bool)
  Q.quickCheck (prop_RightIdentity :: BoolDisj -> Bool)
  Q.quickCheck (prop_Associative :: Or (Sum Int) (Sum Int) -> Or (Sum Int) (Sum Int) -> Or (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_LeftIdentity :: Or (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_RightIdentity :: Or (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_CombineAssociative :: (Int -> (Sum Int)) -> (Int -> (Sum Int)) -> (Int -> (Sum Int)) -> Int -> Bool)
  Q.quickCheck (prop_CombineLeftIdentity :: (Int -> (Sum Int)) -> Int -> Bool)
  Q.quickCheck (prop_CombineRightIdentity :: (Int -> (Sum Int)) -> Int -> Bool)
  Q.quickCheck (prop_CompAssociative :: ((Sum Int) -> (Sum Int)) -> ((Sum Int) -> (Sum Int)) -> ((Sum Int) -> (Sum Int)) -> (Sum Int) -> Bool)
  Q.quickCheck (prop_Associative :: Validation (Sum Int) (Sum Int) -> Validation (Sum Int) (Sum Int) -> Validation (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_Associative :: AccumulateRight (Sum Int) (Sum Int) -> AccumulateRight (Sum Int) (Sum Int) -> AccumulateRight (Sum Int) (Sum Int) -> Bool)
  Q.quickCheck (prop_Associative :: AccumulateBoth (Sum Int) (Sum Int) -> AccumulateBoth (Sum Int) (Sum Int) -> AccumulateBoth (Sum Int) (Sum Int) -> Bool)
