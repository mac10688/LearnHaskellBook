module Ip4AddressParser where
--6. Write a parser for IPv4 addresses.
import Data.Word
import Text.Read
import Text.Trifecta

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

data Octet = MkOctet Word32

ipAddressParser :: Parser IPAddress
ipAddressParser = do
    firstOctet <- octetParser
    skipPeriod
    secondOctet <- octetParser
    skipPeriod
    thirdOctet <- octetParser
    skipPeriod
    fourthOctet <- octetParser
    return $ createIpAddress firstOctet secondOctet thirdOctet fourthOctet

octetParser :: Parser Octet
octetParser = do
    x <- (readMaybe <$> some digit :: Parser (Maybe Word32))
    case x of
        Just x -> if x > 255 
                  then (unexpected ">255") 
                  else return $ MkOctet x
        Nothing -> unexpected "Not a number"

skipPeriod :: Parser ()
skipPeriod = do
    char '.'
    return ()

createIpAddress :: Octet -> Octet -> Octet -> Octet -> IPAddress
createIpAddress (MkOctet o4) (MkOctet o3) (MkOctet o2) (MkOctet o1) = 
    IPAddress $ (o4*256^3)+(o3*256^2)+(o2*256)+o1

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


