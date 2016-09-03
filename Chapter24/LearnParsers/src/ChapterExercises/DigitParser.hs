module DigitParser where

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


