module PhoneNumberParser where

-- 4. Write a parser for US/Canada phone numbers with varying formats.

type NumberingPlanArea = Int -- aka area code
type Exchange = Int
type LineNumber = Int

data PhoneNumber =
    PhoneNumber NumberingPlanArea Exchange LineNumber
    deriving (Eq, Show)

parsePhone :: Parser PhoneNumber
parsePhone =  

takeLastSevenInts :: Parser Int
takeLastSevenInts = takeLast 7 <$> pullOutInts

takeLast :: Int -> [a] -> [a]
takeLast n = reverse . take n . reverse

pullOutInts :: Parser [Char]
pullOutInts = some $ (skipMany nonInt) >> digit 

base10Int :: Parser Integer
base10Int = fromInteger <$> base10Integer'

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


