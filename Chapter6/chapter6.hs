import Data.List
-- data Mood = Blah

-- instance Show Mood where
-- show _ = "Blah"

class Numberish a where
  fromNumber :: Integer -> a
  toNumber :: a -> Integer

newtype Age =
  Age Integer
  deriving (Eq, Show)

instance Numberish Age where
  fromNumber n = Age n
  toNumber (Age n) = n

newtype Year =
  Year Integer
  deriving (Eq, Show)

instance Numberish Year where
  fromNumber n = Year n
  toNumber (Year n) = n

sumNumberish :: Numberish a => a -> a -> a
sumNumberish a a' = fromNumber summed
  where integerOfA = toNumber a
        integerOfAPrime = toNumber a'
        summed = integerOfA + integerOfAPrime

-- data Trivial = Trivial'

data Trivial = Trivial'
instance Eq Trivial where
  Trivial' == Trivial' = True

-- data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun

data Date = Date DayOfWeek Int

-- instance Eq DayOfWeek where
--   (==) Mon Mon = True
--   (==) Tue Tue = True
--   (==) Weds Weds = True
--   (==) Thu Thu = True
--   (==) Fri Fri = True
--   (==) Sat Sat = True
--   (==) Sun Sun = True
--   (==) _ _ = False

instance Eq Date where
  (==) (Date weekday monthNum)
      (Date weekday' monthNum') =
      weekday == weekday' && monthNum == monthNum'

-- f :: Int -> Bool
-- f 1 = True
-- f _ = True

data Identity a = Identity a

instance Eq a => Eq (Identity a) where
  (==) (Identity v) (Identity v') = v == v'

data TisAnInteger = TisAn Integer

instance Eq TisAnInteger where
  (==) (TisAn  v) (TisAn  v') = v == v'

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
  (==) (Two x x') (Two y y') = (x,x')==(y,y')

data StringOrInt =
    TisAnInt Int
  | TisAString String

instance Eq StringOrInt where
  (TisAnInt x) == (TisAnInt x') = x == x'
  (TisAString x) == (TisAString x') = x == x'
  _ ==  _ = False

data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
  (Pair x x') == (Pair y y') = (x, x') == (y, y')

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
  (Tuple x x') == (Tuple y y') = (x,x')==(y,y')

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
  (ThisOne x) == (ThisOne x') = x == x'
  (ThatOne x) == (ThatOne x') = x == x'
  _ == _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
  (Hello x) == (Hello x') = x == x'
  (Goodbye x) == (Goodbye x') = x == x'
  _ == _ = False

data DayOfWeek = Mon | Tue | Weds | Thu | Fri | Sat | Sun
  deriving (Eq, Show)

instance Ord DayOfWeek where
  compare Fri _ = GT
  compare _ Fri = LT
  compare _ _ = EQ

data Person = Person Bool deriving (Show)

printPerson :: Person -> IO()
printPerson person = putStrLn (Prelude.show person)

data Mood = Blah | Woot deriving (Eq, Show)

settleDown x = if (x == Woot)
               then Blah
               else x

type Subject = String
type Verb = String
type Object = String

data Sentence = Sentence Subject Verb Object deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"

data Rocks = Rocks String deriving (Eq, Show)

data Yeah = Yeah Bool deriving (Eq, Show)

data Papu = Papu Rocks Yeah deriving (Eq, Show)

-- phew = Papu "chases" True

truth = Papu (Rocks "chomskydoz")(Yeah True)

equalityForall :: Papu -> Papu -> Bool
equalityForall p p' = p == p'

-- comparePapus :: Papu -> Papu -> Bool
-- comparePapus p p' = p > p'
--
-- i :: Num a => a
-- i :: a
-- i = 1 -- True --False

-- -- f :: Float
-- f :: Num a => a
-- f = 1.0 -- False

-- --f :: Float
-- f :: Fractional a => a
-- f = 1.0 --True

-- -- f :: Float
f :: RealFrac a => a
f = 1.0 --True

-- -- freud :: a -> a
freud :: Ord a => a -> a
freud x = x --True

-- -- freud' :: a -> a
freud' :: Int -> Int
freud' x = x --True

myX = 1 :: Int

-- sigmund :: Int -> Int
-- sigmund :: a -> a
-- sigmund x = myX -- False

-- -- sigmund' :: Int -> Int
-- sigmund' :: Num a => a -> a
-- sigmund' x = myX --True --False

-- -- jung :: Ord a => [a] -> a
jung :: [Int] -> Int
jung xs = head (sort xs) --True

-- -- young :: [Char] -> Char
young :: Ord a => [a] -> a
young xs = head (sort xs) --True

mySort :: [Char] -> [Char]
mySort = sort --True

-- -- signifier :: [Char] -> Char
signifier :: Ord a => [a] -> a
signifier xs = head (mySort xs) --False
