module PhoneNumberParser where

import Text.Trifecta
import Control.Applicative

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
pullOutInts = some $ (skipMany nonInt) >> digit 

nonInt :: Parser Char
nonInt = (char ' ') <|> (char '(') <|> (char '-')

--With the following behavior:
--Prelude> parseString parsePhone mempty "123-456-7890"
--Success (PhoneNumber 123 456 7890)
--Prelude> parseString parsePhone mempty "123456789"
--Success (PhoneNumber 123 456 7890)
--Prelude> parseString parsePhone mempty "(123) 456-7890"
--Success (PhoneNumber 123 456 7890)
--Prelude> parseString parsePhone mempty "1-123-456-7890"
--Success (PhoneNumber 123 456 7890)


