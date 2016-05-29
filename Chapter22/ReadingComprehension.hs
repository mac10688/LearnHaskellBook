{-# LANGUAGE InstanceSigs #-}
import Control.Applicative (liftA2)

newtype Reader r a =
    Reader { runReader :: r -> a }

myLiftA2 :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
myLiftA2 f fa fb = f <$> fa <*> fb 

asks :: (r -> a) -> Reader r a
asks f = Reader $ f

instance Functor (Reader r) where
    fmap :: (a -> b) -> Reader r a -> Reader r b
    fmap f (Reader ra) = Reader $ (f . ra)

instance Applicative (Reader r) where
    pure :: a -> Reader r a
    pure a = Reader $ \r -> a

    (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
    (Reader rab) <*> (Reader ra) = Reader $ \r -> (rab r (ra r))


newtype HumanName =
    HumanName String
    deriving (Eq, Show)

newtype DogName =
    DogName String
    deriving (Eq, Show)

newtype Address =
    Address String
    deriving (Eq, Show)

data Person =
    Person {
        humanName :: HumanName
       ,dogName :: DogName
       ,address :: Address
    } deriving (Eq, Show)

data Dog =
    Dog {
        dogsName :: DogName
       ,dogsAddress :: Address
    } deriving (Eq, Show)

pers :: Person
pers =
    Person (HumanName "Big Bird")
           (DogName "Barkley")
           (Address "Sesame Street")

chris :: Person
chris = Person (HumanName "Chris Allen")
               (DogName "Papu")
               (Address "Austin")


-- without Reader
getDog :: Person -> Dog
getDog p =
    Dog (dogName p) (address p)

-- with Reader
getDogR :: Person -> Dog
getDogR =
    Dog <$> dogName <*> address

getDogR' :: Person -> Dog
getDogR' =
    liftA2 Dog dogName address

getDogR'' :: Reader Person Dog
getDogR'' = Reader getDogR
    
-- Don't forget instancesigs.

instance Monad (Reader r) where
    return = pure

    (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
    (Reader ra) >>= aRb = Reader $ \r -> (runReader $ aRb (ra r)) r 


-- first :: Reader String Int
-- first = Reader length

-- second :: Int -> Reader String Bool
-- second n = Reader $ \s -> length s == n


-- combined :: Reader String Int -> (Int -> Reader String Bool) -> Reader String Bool
-- combined f1 f2 = Reader $ \r -> f2 (f1 r) 

-- runReader first "hello"
-- (runReader $ second 2) "hello"

-- combined
-- (runReader $ second (runReader first "hello") "hello"
-- (runReader $ second (runReader first r ) r
--
