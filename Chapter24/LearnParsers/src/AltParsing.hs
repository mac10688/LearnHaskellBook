{-# LANGUAGE QuasiQuotes #-}
module AltParsing where

import Control.Applicative
import Text.Trifecta
import Text.RawString.QQ

type NumberOrString = Either Integer String
-- type DecimalOrFraction = Either 

eitherOr :: String
eitherOr = [r|
123
abc
456
def
|]


a = "blah"
b = "123"
c = "123blah789"

parseNos :: Parser NumberOrString
parseNos = (Left <$> integer) <|> (Right <$> some letter)

parseNos' :: Parser NumberOrString
parseNos' = do
    skipMany (oneOf "\n") 
    v <- (Left <$> integer) <|> (Right <$> some letter)
    skipMany (oneOf "\n")
    return v


main = do
    print $ parseString (some letter) mempty a
    print $ parseString integer mempty b
    print $ parseString parseNos mempty a
    print $ parseString parseNos mempty b
    print $ parseString (many parseNos) mempty c
    print $ parseString (some parseNos) mempty c

main' = do
    print $ parseString (some parseNos') mempty eitherOr



