module Main where

import Prelude
import Control.Applicative ((<|>))
import Data.Char
import Data.List
import Data.Word
import Text.Trifecta
import Test.Hspec
import Text.Parser.LookAhead (lookAhead)

data IPAddress =
  IPAddress Word32
  deriving (Eq, Ord, Show)

-- a.b.c.d
-- = (a * 256^3) + (b * 256^2) + (c * 256) + d

pIP :: Parser IPAddress
pIP = do
  let failTooHigh x = if x > 255 then (unexpected ">255") else (return ())
  a <- decimal
  failTooHigh a
  char '.'
  b <- decimal
  failTooHigh b
  char '.'
  c <- decimal
  failTooHigh c
  char '.'
  d <- decimal
  failTooHigh d
  return $ IPAddress ((fromIntegral a) * 256^3 + (fromIntegral b) * 256^2 + (fromIntegral c) * 256 + (fromIntegral d))

number :: TokenParsing m => Integer -> m Char -> m Integer
number base baseDigit =
  foldl' (\x d -> base * x + toInteger(digitToInt d)) 0 <$> some baseDigit

hex :: TokenParsing m => m Integer
hex = number 16 hexDigit

maybeSuccess :: Result a -> Maybe a
maybeSuccess (Success a) = Just a
maybeSuccess _ = Nothing

testIPAddress4 = hspec $ do
    describe "happyPath" $ do
        let ipAddress = "172.16.254.1"
        it ipAddress $ do
            let parsedIpAddress = parseString pIP mempty ipAddress
                r' = maybeSuccess parsedIpAddress
            r' `shouldBe` (Just $ IPAddress 2886794753)
    describe "Another happy path" $ do
        let ipAddress = "204.120.0.15"
        it ipAddress $ do
            let parsedIpAddress = parseString pIP mempty ipAddress
                r' = maybeSuccess parsedIpAddress
            r' `shouldBe` (Just $ IPAddress 3430416399)
    describe "Too high" $ do
        let ipAddress = "172.256.256.256"
        it ipAddress $ do
            let parsedIpAddress = parseString pIP mempty ipAddress
                r' = maybeSuccess parsedIpAddress
            r' `shouldBe` Nothing
    describe "Too low" $ do
        let ipAddress = "172.-1.255.255"
        it ipAddress $ do
            let parsedIpAddress = parseString pIP mempty ipAddress
                r' = maybeSuccess parsedIpAddress
            r' `shouldBe` Nothing

data IPAddress6 =
  IPAddress6 Word64 Word64
  deriving (Eq,Ord,Show)

-- a:b:c:d:e:f:g:h
-- (a * (2^16)^3) + (b * (2^16)^2) + (c * (2^16)) + d
-- (e * (2^16)^3) + (f * (2^16)^2) + (g * (2^16)) + h

-- "abcd:abcd" -> ["abcd", "abcd"]
pIPv6NonEmptyCell :: Parser (Maybe Integer)
pIPv6NonEmptyCell = do
  h <- hex
  lookAhead $ (do
    notFollowedBy ((try $ char ':' >> return ()) <|> eof)
    unexpected "not :, eof, or hexadecimal") <|> (((try $ char ':' >> return ()) <|> eof))
  if h > cellUpperbound then (unexpected $ "Expected a value between 0 and " ++ (show cellUpperbound) ++ " got " ++ (show h)) else (return ())
  return $ Just h

pIPv6EmptyCell :: Parser (Maybe Integer)
pIPv6EmptyCell = do
  lookAhead $ (char ':' >> return ()) <|> eof
  return Nothing

testpIPv6EmptyCell = hspec $ do
    describe "Testing Empty Cell" $ do
        let testString = ":"
        it testString $ do
            let emptyCell = parseString pIPv6EmptyCell mempty testString
                r' = maybeSuccess emptyCell
            r' `shouldBe` (Just Nothing)
    describe "Testing Non"
        let testString = eof
        it test

pIPv6Cells :: Parser [Maybe Integer]
pIPv6Cells = (pIPv6NonEmptyCell <|> pIPv6EmptyCell) `sepBy` (char ':')

expander :: Integer -> Maybe Integer -> [Integer]
expander n maybeInteger = case maybeInteger of
  Just x  -> [x]
  Nothing -> replicate (fromIntegral n) 0

{-
cIPv6 :: [Maybe Integer] -> [Integer]
cIPv6 cells =
  let emptyCells = 8 - (length cells)
      expanded =
-}

pIP6dumb :: Parser IPAddress6
pIP6dumb = do
  a <- hex
  char ':'
  b <- hex
  char ':'
  c <- hex
  char ':'
  d <- hex
  char ':'
  e <- hex
  char ':'
  f <- hex
  char ':'
  g <- hex
  char ':'
  h <- hex
  return $ IPAddress6
    ((fromIntegral a) * (2^(16*3)) + (fromIntegral b) * (2^(16*2)) + (fromIntegral c) * (2^16) + (fromIntegral d))
    ((fromIntegral e) * (2^(16*3)) + (fromIntegral f) * (2^(16*2)) + (fromIntegral g) * (2^16) + (fromIntegral h))


main :: IO ()
main = undefined
-- main = do
--   putStrLn "idunno" testIPAddress6 = hspec $ do
--     describe "happyPath" $ do
--         let ipAddress = "0:0:0:0:0:ffff:ac10:fe01"
--         it ipAddress $ do
--             let parsedIpAddress = parseString pIP mempty ipAddress
--                 r' = maybeSuccess parsedIpAddress
--             r' `shouldBe` (Just $ IPAddress6 )
--     describe "Another happy path" $ do
--         let ipAddress = "204.120.0.15"
--         it ipAddress $ do
--             let parsedIpAddress = parseString pIP mempty ipAddress
--                 r' = maybeSuccess parsedIpAddress
--             r' `shouldBe` (Just $ IPAddress 3430416399)
--     describe "Too high" $ do
--         let ipAddress = "172.256.256.256"
--         it ipAddress $ do
--             let parsedIpAddress = parseString pIP mempty ipAddress
--                 r' = maybeSuccess parsedIpAddress
--             r' `shouldBe` Nothing
--     describe "Too low" $ do
--         let ipAddress = "172.-1.255.255"
--         it ipAddress $ do
--             let parsedIpAddress = parseString pIP mempty ipAddress
--                 r' = maybeSuccess parsedIpAddress
--             r' `shouldBe` Nothing

