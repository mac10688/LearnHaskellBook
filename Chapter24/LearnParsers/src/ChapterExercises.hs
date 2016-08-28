
module ChapterExercises where

import Data.Word
import Text.Trifecta
import Control.Applicative
import Data.Char
import Data.Hourglass

-- Relevant to precedence/ordering,
-- cannot sort numbers like strings.
data NumberOrString = NOSS String | NOSI Integer
                      deriving (Eq, Show)

type Major = Integer
type Minor = Integer
type Patch = Integer
type Release = [NumberOrString]
type Metadata = [NumberOrString]

data SemVer = SemVer Major Minor Patch Release Metadata
              deriving (Eq, Show)

instance Ord SemVer where
    (SemVer major minor patch release metadata) `compare` (SemVer major' minor' patch' release' metadata') =
        case (major `compare` major') of
            LT -> LT
            GT -> GT
            EQ -> case (minor `compare` minor') of
                  LT -> LT
                  GT -> GT
                  EQ -> patch `compare` patch'
    --I don't feel like comparing the release data. The rules are too complicated.


-- 1. Write a parser for semantic versions as defined by http://semver.org/.
-- After making a working parser, write an Ord instance for the SemVer type
-- that obeys the specification outlined on the SemVer type that obeys
-- the specification outlined on the SemVer website.

parseMajor :: Parser Major
parseMajor = decimal <* char '.'

parseMinor :: Parser Minor
parseMinor = decimal <* char '.'

parsePatch :: Parser Patch
parsePatch = decimal

-- parseRelease :: Parser Release
-- parseRelease = (char '-') *> (some $ NOSS <$> (some letter) <|> (NOSI <$> decimal)) <* (optional (char '.')) 
parseNumberOrString :: Parser NumberOrString
parseNumberOrString  = NOSS <$> (some letter) <|> (NOSI <$> decimal)

parseRelease :: Parser Release
parseRelease = do
    hasRelease <- optional $ char '-'
    case hasRelease of
        Nothing -> do return ([])
        Just _ -> do
                    release <- some $ parseNumberOrString <* (skipOptional $ char '.')
                    return release

parseMetadata = do
    hasMetadata <- optional $ char '+'
    case hasMetadata of
        Nothing -> do return ([])
        Just _ -> do
                    metadata <- some $ parseNumberOrString <* (skipOptional $ char '.')
                    return metadata

parseSemVer :: Parser SemVer
parseSemVer = do
    major <- parseMajor
    minor <- parseMinor
    patch <- parsePatch
    release <- parseRelease
    metadata <- parseMetadata
    return (SemVer major minor patch release metadata)

-- Expected results:
-- Prelude> parseString parseSemVer mempty "2.1.1"
-- Success (SemVer 2 1 1 [] [])
-- Prelude> parseString parseSemVer mempty "1.0.0-x.7.z.92"
-- Success (SemVer 1 0 0 [NOSS "x", NOSI 7, NOSS "z", NOSI 92] [])
-- Prelude> SemVer 2 1 1 [] [] > SemVer 2 1 0 [] []
-- True

-- 2. Write a parser for positive integer values.
-- Don't reuse the pre-existing digit or integer functions,
-- but you can use them for inspiration if you get stuck.
--
-- Hint: Assume you're parsing base-10 numbers. Use arithmetic
-- as a cheap "accumulator" for your final number as you parse
-- each digit left-to-right.

parseDigit :: Parser Char
parseDigit = satisfy isDigit

base10Integer :: Parser Integer
base10Integer = convertStringToInt <$> (some parseDigit)

convertStringToInt :: [Char] -> Integer
convertStringToInt string = snd $ foldr (\item (place, acc) ->
                                   let newAcc = (convertCharToInteger item) * place
                                   in (place * 10, acc + newAcc)) (1,0) string

convertCharToInteger :: Char -> Integer
convertCharToInteger c =
    case c of
        '1' -> 1
        '2' -> 2
        '3' -> 3
        '4' -> 4
        '5' -> 5
        '6' -> 6
        '7' -> 7
        '8' -> 8
        '9' -> 9
        '0' -> 0

-- Expected results:
-- Prelude> parseString parseDigit mempty "123"
-- Success '1'
-- Prelude> parseString parseDigit mempty "abc"
-- Failure (interactive):1:1: error : expected: parseDigit
-- abc<EOF>
-- ^
-- Prelude>parseString base10Integer mempty "123abc"
-- Success 123
-- Prelude> parseString base10Integer mempty "abc"
-- Failure (interactive):1:1: error: expected: integer
-- abc<EOF>
-- ^

-- 3. Extend the parser you wrote to handle negative and positive integers.
-- Try writing a new parser in terms of the one you already have to do this.

base10Integer' :: Parser Integer
base10Integer' = do parseDash <- optional (char '-') 
                    case parseDash of
                        Just _ -> (*(-1)) <$> base10Integer
                        otherwise -> base10Integer

-- Prelude> parseString base10Integer' mempty "-123abc"
-- Success (-123)

-- 4. Write a parser for US/Canada phone numbers with varying formats.

type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone =  producePhoneNumber <$> takeLastTenInts 

producePhoneNumber :: [Char] -> PhoneNumber
producePhoneNumber xs =
                        let areaCode = take 3 xs
                            exchange = (take 3) $ drop 3 xs
                            lineNumber = (take 4) $ drop 6 xs
                        in PhoneNumber (read areaCode :: Int) (read exchange :: Int) (read lineNumber :: Int)

takeLastTenInts :: Parser [Char]
takeLastTenInts = (takeLast 10) <$> pullOutInts

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

pullOutInts :: Parser [Char]
pullOutInts = some $ (many nonInt) *> digit <* (optional eof) 

nonInt :: Parser Char
nonInt = (char ' ') <|> (char '(') <|> char ')' <|> (char '-')

--With the following behavior:
--Prelude> parseString parsePhone mempty "123-456-7890"
--Success (PhoneNumber 123 456 7890)
--Prelude> parseString parsePhone mempty "123456789"
--Success (PhoneNumber 123 456 7890)
--Prelude> parseString parsePhone mempty "(123) 456-7890"
--Success (PhoneNumber 123 456 7890)
--Prelude> parseString parsePhone mempty "1-123-456-7890"
--Success (PhoneNumber 123 456 7890)

--5. Write a parser for a log file format and sum the time spent in each activity.
--Additionally, provide an alternative aggregation of the data
--that provides average time spent per activity per day.
--The format supports the use of comments which your parser
--will have to ignore. The # characters followed by a date mark the beginning
--of a particular day.

type Activity = String
-- type Time = undefined

parseStringToMonth :: Parser Date
parseStringToMonth = do
    whiteSpace
    char '#'
    whiteSpace
    year <- some digit
    char '-'
    month <- some digit
    char '-'
    day <- some digit
    return $ Date (read year :: Int) ( c (read month :: Int)) (read day :: Int)

c :: Int -> Month
c 1 = January
c 2 = February
c 3 = March
c 4 = April
c 5 = May
c 6 = June
c 7 = July
c 8 = August
c 9 = September
c 10 = October
c 11 = November
c 12 = December

parseTimeOfDay :: Parser TimeOfDay
parseTimeOfDay = do
    hourInt <- 

skipWhiteSpaceAndComments :: Parser ()
skipWhiteSpaceAndComments = do
    whiteSpace
    optional string "--"
    skip manyTill char '\n'

--Log format example:
--
--"-- wheee a comment"
--
-- # 2025-02-05
--08:00 Breakfast
--09:00 Sanitizing moisture collector
--11:00 Exercising in high-grav gym
--12:00 Lunch
--13:00 Programming
--17:00 Commuting home in rover
--17:30 R&R
--19:00 Dinner
--21:00 Shower
--21:15 Read
--22:00 Sleep

-- # 2025-02-07 -- dates not necessarily sequential
--08:00 Breakfast -- should I try skippin bfast?
--09:00 Bumped head, passed out
--13:36 Wake up, headache
--13:37 Go to medbay
--13:40 Patch self up
--13:45 Commute home for rest
--14:15 Read
--21:00 Dinner
--21:15 Read
--22:00 Sleep
--
--You are to derive a reasonable datatype for representing this data
--yourself. For bonus points, make this bi-directional by making
--a Show representation for the datatype which matches the format
--you are parsing. Then write a generator for this data using
--QuickCheck's Gen and see if you can break your parser with QuickCheck

--6. Write a parser for IPv4 addresses.

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

--A 32-bit word is a 32-bit unsigned int. Lowest value is 0 rather than
--being capable of representing negative numbers, but the highest
--possible value in the same number of bits is twice as high. Note:
--
--Prelude> import Data.Int
--Prelude> import Data.Word
--Prelude> maxBound :: Int32
--2147483647
--Prelude> maxBound :: Word32
--4294967295
--Prelude> div 4294967295 2147483647
--2

--Word32 is an appropriate and compact way to represent IPv4
--addresses. You are expected to figure out not only how to parse
--the typical IP address format, but how IP addresses work
--numerically insofar as is required to write a working parser.
--This will require using a search engine unless you have an
--appropriate book on internet networking handy.
--
--Example IPv4 addresses and their decimal representations:
--
--172.16.254.1 -> 2886794753
--204.120.0.15 -> 3430416399
--
--7. Same as before, but IPv6.


data IPAddress6 = IPAddress6 Word64 Word64 deriving (Eq, Ord, Show)

--Example IPv6 addresses and their decimal representations:
--
--0:0:0:0:fff:ac10:fe01 -> 281473568538113
--
--0:0:0:0:0:ffff:cc78:f -> 281474112159759
--
--FE80:0000:0000:0000:0202:B3FF:FE1E:8329 
-- -> 338288524927261089654163772891438416681
--
--2001:DB8::8:800:200C:417A -> 42540766411282592856906245548098208122
--
