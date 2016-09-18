module Ip6AddressParser where
--7. Same as before, but IPv6.

import Text.Trifecta
import Data.Word
import Numeric
import Control.Monad
import Data.List

data IP6Address = IP6Address Word64 Word64 deriving (Eq, Ord, Show)

data IP6Cell = EmptyCell | Cell Word deriving (Eq, Ord, Show)


parseIP6Address :: Parser IP6Address
parseIP6Address = do 
    cells <- (fmap sequenceA) $ (fmap . fmap) cStringToIP6Cell parserOctetStrings
    case cells of
        Nothing -> unexpected ("Malformed IPv6 Address")
        Just xs -> return $ ip6CellsToIP6Address xs


ip6CellsToIP6Address :: [IP6Cell] -> IP6Address 
ip6CellsToIP6Address xs = 
        let 
            expandedCells = expandIP6Cells xs
            convertToWord = convertIP6CellsToWords expandedCells
            (firstWord, secondWord) = wordsToDoubleWords convertToWord
        in
           IP6Address firstWord secondWord 
        
expandIP6Cells :: [IP6Cell] -> [IP6Cell]
expandIP6Cells xs =
    let index = elemIndex EmptyCell xs
        lengthOfList = length xs
        numberToExpandTo = 8 - lengthOfList
        (xs,ys) =
            case index of
                Nothing -> (xs,[])
                (Just x) -> splitAt x xs
        replicatedEmpties = replicate numberToExpandTo EmptyCell
    in
        xs ++ replicatedEmpties ++ ys
        
convertIP6CellsToWords :: [IP6Cell] -> [Word]
convertIP6CellsToWords = map convertIP6CellToWord 

convertIP6CellToWord :: IP6Cell -> Word
convertIP6CellToWord (Cell value) = value
convertIP6CellToWord EmptyCell = 0

wordsToDoubleWords :: [Word] -> (Word64, Word64)
wordsToDoubleWords = undefined

parserOctetStrings :: Parser [String]
parserOctetStrings = sepBy (many hexDigit) colon

cStringToIP6Cell :: String -> Maybe IP6Cell
cStringToIP6Cell "" = Just EmptyCell
cStringToIP6Cell x =
    let reversedArray = reverse x
        word8Array = sequenceA $ map hexCharToWord8 reversedArray
    in
        case word8Array of
            Nothing -> Nothing
            Just xs -> Just $ Cell (calculateValue xs)

calculateValue :: [Word8] -> Word
calculateValue xs = snd 
                    $ foldl (\(power, acc) item ->
                            let newAcc = calculate item acc power
                            in (power + 1, newAcc)) (0,0) xs
    where 
        calculate item acc power = (fromIntegral item :: Word)*(16^power)+acc

hexCharToWord8 :: Char -> Maybe Word8
hexCharToWord8 x 
        | x == '1' = Just 1
        | x == '2' = Just 2
        | x == '3' = Just 3
        | x == '4' = Just 4
        | x == '5' = Just 5
        | x == '6' = Just 6
        | x == '7' = Just 7
        | x == '8' = Just 8
        | x == '9' = Just 9
        | x == 'a' || x == 'A' = Just 10
        | x == 'b' || x == 'B' = Just 11
        | x == 'c' || x == 'C' = Just 12
        | x == 'd' || x == 'D' = Just 13
        | x == 'e' || x == 'E' = Just 14
        | x == 'f' || x == 'F' = Just 15
        | otherwise = Nothing

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
--One of the trickier parts about IPv6 will be full vs. collapsed addresses and the abbreviations. See this Q&A thread about IPv6 abbreviations for more.
--Ensure you can parse abbreviated varaitions of the earlier examples like:
--
--FE80::0202:B3FF:FE1E:8329
--2001:DB8::8:800:200C:417A
