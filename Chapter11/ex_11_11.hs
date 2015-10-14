data GuessWhat = Chickenbutt deriving (Eq, Show)

data Id a = MkId a deriving (Eq, Show)

data Product a b = Product a b deriving (Eq, Show)

data Sum a b = First a | Second b deriving (Eq, Show)

data RecordProduct a b = RecordProduct { pfirst :: a, psecond :: b } deriving (Eq, Show)

-- if the counts could overflow,
-- then the farm can afford the
-- programmer time to convert
-- the system

newtype NumCow = NumCow Int deriving (Eq, Show)
newtype NumPig = NumPig Int deriving (Eq, Show)
data Farmhouse = Farmhouse NumCow NumPig deriving (Eq, Show)
type Farmhouse' = Product NumCow NumPig

newtype NumSheep =
  NumSheep Int
  deriving (Eq, Show)

data BigFarmhouse =
  BigFarmhouse NumCow NumPig NumSheep
  deriving (Eq, Show)

type BigFarmhouse' =
  Product NumCow (Product NumPig NumSheep)

-- type Name = String
type Age = Int
type LovesMud = Bool

-- Sheep can produce between 2 and 30
-- pounds of wool per year!
-- Icelandic shep don't produce as much
-- wool per year as other breeds but the
-- wool they do produce is a finer wool

-- type PoundsOfWool = Int
-- data CowInfo = CowInfo Name Age deriving (Eq, Show)
-- data PigInfo = PigInfo Name Age LovesMud deriving (Eq, Show)
-- data SheepInfo = SheepInfo Name Age PoundsOfWool deriving (Eq, Show)
-- data Animal = Cow CowInfo
--             | Pig PigInfo
--             | Sheep SheepInfo
--             deriving (Eq, Show)

-- Alternately

-- type Animal' = Sum CowInfo (Sum PigInfo SheepInfo)

trivialValue :: GuessWhat
trivialValue = Chickenbutt

idInt :: Id Integer
idInt = MkId 10

idIdentity :: Id (a -> a)
idIdentity = MkId $ \x -> x

type Awesome = Bool
-- type Name = String

-- person :: Product Name Awesome
-- person = Product "Simon" True

-- data Twitter = Twitter deriving (Eq, Show)
-- data AskFm = AskFm deriving (Eq, Show)

-- socialNetwork :: Sum Twitter AskFm
-- socialNetwork = First Twitter

data SocialNetwork = Twitter | AskFm deriving (Eq, Show)

type Twitter = String
type AskFm = String

twitter :: Sum Twitter AskFm
twitter = First "Twitter"


-- It has no way of knowing
-- we made a mistake because
-- both values are just Strings
askfm :: Sum Twitter AskFm
askfm = First "Twitter"

myRecord :: RecordProduct Integer Float
myRecord = RecordProduct 42 0.0001

myRecord' :: RecordProduct Integer Float
myRecord' = RecordProduct { pfirst = 42, psecond = 0.00001 }

data OperatingSystem =
    GnuPlusLinux
  | OpenBSDPlusNevermindJustBSDStill
  | Mac
  | Windows
  deriving (Eq, Show)

data ProgrammingLanguage =
    Haskell
  | Agda
  | Idris
  | Purescript
  deriving (Eq, Show)

data Programmer =
  Programmer { os :: OperatingSystem
             , lang :: ProgrammingLanguage }
  deriving (Eq, Show)

nineToFive :: Programmer
nineToFive = Programmer { os = Mac, lang = Haskell }

feelingWizardly :: Programmer
feelingWizardly = Programmer { lang = Agda, os = GnuPlusLinux }

allOperatingSystems :: [OperatingSystem]
allOperatingSystems =
  [ GnuPlusLinux
  , OpenBSDPlusNevermindJustBSDStill
  , Mac
  , Windows
   ]

allLanguages :: [ProgrammingLanguage]
allLanguages = [Haskell, Agda, Idris, Purescript]

allProgrammers :: [Programmer]
allProgrammers = [Programmer { lang = x, os = y } | x <- allLanguages, y <- allOperatingSystems]

-- Works the same as if we'd used record syntax

data ThereYet = There Integer Float String Bool deriving (Eq, Show)

-- who needs a "builder pattern"?
nope :: Float -> String -> Bool -> ThereYet
nope = There 10

notYet :: String -> Bool -> ThereYet
notYet = nope 25.5

notQuite :: Bool -> ThereYet
notQuite = notYet "woohoo"

yussss :: ThereYet
yussss = notQuite False

-- Not I, said the Haskell User

newtype Name = Name String deriving Show
newtype Acres = Acres Int deriving Show

-- FarmerType is a Sum
data FarmerType = DairyFarmer
                | WheatFarmer
                | SoybeanFarmer deriving Show

-- Farmer is a plain ole product of
-- Name, Acres, and FarmerTypedata
data Farmer = Farmer Name Acres FarmerType deriving Show

isDairyFarmer :: Farmer -> Bool
isDairyFarmer (Farmer _ _ DairyFarmer) = True
isDairyFarmer _ = False

data FarmerRec =
  FarmerRec { name :: Name
            , acres :: Acres
            , farmerType :: FarmerType } deriving Show

isDairyFarmerRec :: FarmerRec -> Bool
isDairyFarmerRec farmer = case farmerType farmer of
  DairyFarmer -> True
  _ -> False

-- Don't do this
-- data Automobile = Null
--                 | Car { make :: String
--                       , model :: String
--                       , year :: Integer }
--                 deriving (Eq, Show)

-- Split out the record/product
data Car = Car { make :: String
               , model :: String
               , year :: Integer }
               deriving (Eq, Show)

-- The Null is still not great, but
-- we're leaving it in to make a point
data AutoMobile = Null
                | Automobile Car
                deriving (Eq, Show)
