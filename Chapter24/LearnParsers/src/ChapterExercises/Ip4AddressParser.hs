module Ip4AddressParser.hs where
--6. Write a parser for IPv4 addresses.

data IPAddress = IPAddress Word32 deriving (Eq, Ord, Show)

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
-
