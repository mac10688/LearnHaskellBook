module Main where

import Text.Trifecta
import Text.Parser.Combinators (eof)

stop :: Parser a
stop = unexpected "stop"

-- read a single character '1'
one = char '1' 

-- read a single character '1', then die
one' = one >> stop
-- equivalent to char '1' >> stop

-- char :: Char -> Parser Char
-- char c =
--     Parser $ \ s ->
--         case s of
--             (x:xs) -> if c == x
--                       then [(c, xs)]
--                       else []
--             _ -> []

-- from Text.ParserCombinators.HuttonMeijer
-- polyarse-1.11

-- type Token = Char
-- newtype Parser a = P ([Token] -> [(a, [Token])])

-- Same thing, differently formatted:
-- type Parser' a = String -> [(a, String)]

-- read two characters, '1', and '2'
oneTwo = char '1' >> char '2'

-- read two characters, '1' and '2', then die
oneTwo' = oneTwo >> stop

testParse :: Parser Char -> IO ()
testParse p = print $ parseString p mempty "123"

oneString :: Parser String
oneString = string "1"

oneTwoString :: Parser String
oneTwoString = oneString >> string "2"

oneTwoThreeString :: Parser String
oneTwoThreeString = oneTwoString >> string "3"

testParseString :: Parser String -> IO ()
testParseString p = print $ parseString p mempty "123"

pNL s = putStrLn ('\n' : s)

main = do
    pNL "stop:"
    testParse stop
    pNL "one:"
    testParse one
    pNL "one':"
    testParse one'
    pNL "oneTwo:"
    testParse oneTwo
    pNL "oneTwo':"
    testParse oneTwo'
