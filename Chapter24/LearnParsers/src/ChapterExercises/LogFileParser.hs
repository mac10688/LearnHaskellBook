-- 5. Write a parser for a log file format and sum the time spent in
-- each activity. Additionally, provide an alternative aggregation
-- of the data that provides average time spent per activity per day.
-- The format supports the use of comments which your parser will have
-- to ignore. The # characters followed by a date mark the beginning
-- of a paritcular day.
--
-- Log format example:
--
-- whee a comment
--
-- # 2025-02-05
-- 08:00 Breakfast
-- 09:00 Sanitizing moisture collector
-- 11:00 Exercising in high-grav gym
-- 12:00 Lunch
-- 13:00 Programming
-- 17:00 Commuting home in rover
-- 17:30 R&R
-- 19:00 Dinner
-- 21:00 Shower
-- 21:15 Read
-- 22:00 Sleep
--
-- # 2025-02-07 -- dates not nececessarily sequential
-- 08:00 Breakfast -- should I try skippin bfast?
-- 09:00 Bumped head, passed out
-- 13:36 Wake up, headache
-- 13:37 Go to medbay
-- 13:40 Patch self up
-- 13:45 Commute home for rest
-- 14:15 Read
-- 21:00 Dinner
-- 21:15 Read
-- 22:00 Sleep
--
-- You are to derive a reasonable datatype for representing this
-- data yourself. For bonus points, make this bi-directional by
-- making a Show representation for the datatype which matches
-- the format you are parsing. Then write a generator for this
-- data using QuickChecks' Gen and see if you can break your
-- parser with QuickCheck.
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
module LogFileParser where

import Data.Hourglass
import Text.Trifecta
import Control.Applicative
import Text.RawString.QQ
import Test.Hspec
import Text.Parser.LookAhead

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

data Description = MkDescription String deriving (Eq, Show)

data ActivityRecord = MkActivity TimeOfDay Description deriving (Eq, Show)

data DayRecord = MkDayRecord Date [ActivityRecord] deriving (Eq, Show)

skipWhiteSpaceAndComments :: Parser ()
skipWhiteSpaceAndComments = do
    optional spaces
    parsedComment <- (optional comment)
    case parsedComment of
        Just _ -> skipWhiteSpaceAndComments
        Nothing -> return ()
   


comment :: Parser()
comment = do
    string "--"
    skipMany (noneOf "\n")
    char '\n'
    return ()


parseDate :: Parser Date
parseDate = do
    year <- read <$> some digit
    char '-'
    month <- read <$> some digit
    char '-'
    day <- read <$> some digit
    return $ Date year (c month) day
    where c m =
            case m of
                1 -> January
                2 -> February
                3 -> March
                4 -> April
                5 -> May
                6 -> June
                7 -> July
                8 -> August
                9 -> September
                10 -> October
                11 -> November
                12 -> December

parseTime :: Parser TimeOfDay
parseTime = do
    hour <- read <$> some digit
    char ':'
    minute <- read <$> some digit
    return $ TimeOfDay (Hours hour) (Minutes minute) 0 0

parseDescription :: Parser Description
parseDescription =  MkDescription <$> manyTill anyChar (try (lookAhead endParser))
                    where endParser = string "--" <|> string "\n"


parseActivityRecord :: Parser ActivityRecord
parseActivityRecord = do
    time <- parseTime
    whiteSpace
    description <- parseDescription
    return $ MkActivity time description

parseDayRecord :: Parser DayRecord
parseDayRecord = do
    skipWhiteSpaceAndComments
    string "# "
    date <- parseDate
    activities <- some (skipWhiteSpaceAndComments *> 
                        parseActivityRecord <* 
                        skipWhiteSpaceAndComments)
    return $ MkDayRecord date activities

parseLog :: Parser [DayRecord]
parseLog = many parseDayRecord

-- Tests --

whiteSpaceString = [r|

-- This is a comment

|]

testSkipWhiteSpaceAndCommentsParser :: IO ()
testSkipWhiteSpaceAndCommentsParser = hspec $ do
    describe "Testing white space and comment parser" $ do
        it whiteSpaceString $ do
            let parsedString = parseString (skipWhiteSpaceAndComments) mempty whiteSpaceString
                r' = maybeSuccess parsedString
            r' `shouldBe` Just ()

testCommentString = [r|-- This is a comment|]

testCommentParsing :: IO ()
testCommentParsing = hspec $ do
    describe "Testing comment parsing" $ do
        it testCommentString $ do
            let parsedComment = parseString comment mempty testCommentString
                r' = maybeSuccess parsedComment
            r' `shouldBe` Just ()

testDateString = [r|1988-10-06|]

testDateParsing :: IO()
testDateParsing = hspec $ do
    describe "Testing date parsing" $ do
        it testDateString $ do
            let parsedDate = parseString parseDate mempty testDateString
                r' = maybeSuccess parsedDate
            r' `shouldBe` (Just $ Date 1988 October 6)


testTimeString = [r|10:06|]

testTimeParsing :: IO()
testTimeParsing = hspec $ do
    describe "Testing time parsing" $ do
        it testTimeString $ do
            let parsedTime = parseString parseTime mempty testTimeString
                r' = maybeSuccess parsedTime
            r' `shouldBe` (Just $ TimeOfDay 10 6 0 0)

testDescriptionString = [r|This is a test --Here is a comment|]

testDescriptionParsing :: IO()
testDescriptionParsing = hspec $ do
    describe "Testing description with a comment" $ do
        it testDescriptionString $ do
            let parsedDescription = parseString parseDescription mempty testDescriptionString
                r' = maybeSuccess parsedDescription
            r' `shouldBe` (Just $ MkDescription "This is a test ")

testActivityRecordString = [r|08:10 Running to the store to grab some stuff --This is comment|]

testActivityRecordTest :: IO()
testActivityRecordTest = hspec $ do
    describe "Parse activity with a comment" $ do
        it testActivityRecordString $ do
            let parsedActivity = parseString parseActivityRecord mempty testActivityRecordString
                r' = maybeSuccess parsedActivity
                timeOfDay = TimeOfDay (Hours 8) (Minutes 10) 0 0
                description = MkDescription "Running to the store to grab some stuff "
            r' `shouldBe` (Just $ MkActivity timeOfDay description)

testDayRecordString = [r|
--This is a comment

# 2025-02-05 -- dates not necessarily sequential

08:00 Breakfast -- should I try skippin bfast?
09:00 Sanitizing moisture collector
11:00 Exercising in high-grav gym
|]

testDayRecordExpectedResult :: DayRecord
testDayRecordExpectedResult =
    let date = Date 2025 February 05
        time1 = TimeOfDay (Hours 8) (Minutes 0) 0 0
        description1 = MkDescription "Breakfast "
        firstActivity = MkActivity time1 description1
        time2 = TimeOfDay (Hours 9) (Minutes 0) 0 0
        description2 = MkDescription "Sanitizing moisture collector"
        secondActivity = MkActivity time2 description2
        time3 = TimeOfDay (Hours 11) (Minutes 0) 0 0
        description3 = MkDescription "Exercising in high-grav gym"
        thirdActivity = MkActivity time3 description3
    in
        MkDayRecord date [firstActivity, secondActivity, thirdActivity]


testDayRecordParser :: IO()
testDayRecordParser = hspec $ do
    describe "Parsing day record" $ do
        it testDayRecordString $ do
            let parsedDayRecord = parseString parseDayRecord mempty testDayRecordString
                r' = maybeSuccess parsedDayRecord
            r' `shouldBe` (Just testDayRecordExpectedResult)


