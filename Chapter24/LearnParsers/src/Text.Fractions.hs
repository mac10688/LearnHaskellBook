{-# LANGUAGE OverloadedStrings #-}

module Text.Fractions where

import Control.Applicative
import Data.Attoparsec.Text (parseOnly)
import Data.Ratio ((%))
import Data.String (IsString)
import Text.Trifecta

badFraction :: IsString s => s
badFraction = "1/0"

alsoBad :: IsString s => s
alsoBad = "10"

shouldWork :: IsString s => s
shouldWork = "1/2"

shouldAlsoWork :: IsString s => s
shouldAlsoWork = "2/1"

--actual parser
-- parseFraction :: Parser Rational
-- parseFraction = do
--     numerator <- decimal
--     char '/'
--     denominator <- decimal
--     return (numerator % denominator)



parseDecimal :: Parser Double
parseDecimal = double

parseFraction :: (Monad m, TokenParsing m) => m Rational
parseFraction = do
    numerator <- decimal
    char '/'
    denominator <- decimal
    case denominator of
        0 -> fail "Denominator cannot be zero"
        _ -> return (numerator % denominator)

parseDecimalOrFraction :: Parser (Either Double Rational)
parseDecimalOrFraction = do
    v <- (Left <$> try double) <|> (Right <$> try parseFraction)
    return v

main :: IO()
main = do
    -- parseOnly is Attoparsec
    print $ parseOnly parseFraction badFraction
    print $ parseOnly parseFraction shouldWork
    print $ parseOnly parseFraction shouldAlsoWork
    print $ parseOnly parseFraction alsoBad

    -- parseString is Trifecta
    print $ parseString parseFraction mempty badFraction
    print $ parseString parseFraction mempty shouldWork
    print $ parseString parseFraction mempty shouldAlsoWork
    print $ parseString parseFraction mempty alsoBad

