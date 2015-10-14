{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

data Example = MakeExample deriving Show
data Example' = MakeExample' Int  deriving Show

class TooMany a where
  tooMany :: a -> Bool

--instance TooMany Int where
--  tooMany n = n > 42

newtype Goats = Goats Int deriving Show

instance TooMany Goats where
  tooMany (Goats n) = n > 43

type Goats' = Int

instance TooMany Goats' where
  tooMany a = a > 43

newtype Goats'' = Goats'' Int deriving (Eq, Show)

instance TooMany Goats'' where
  tooMany (Goats'' n) = tooMany n

newtype Goats''' = Goats''' Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
  tooMany (a,b) = a > 43

--instance TooMany (Int, Int) where
--  tooMany (a,b) = (a+b)>43

instance (Num a, TooMany a) => TooMany (a,a) where
  tooMany (a, b) = (tooMany a) || (tooMany a)
