import Data.Time

data DatabaseItem = DbString String | DbNumber Integer | DbDate UTCTime deriving (Eq, Ord, Show)

theDatabase :: [DatabaseItem]
theDatabase =
  [DbDate (UTCTime
          (fromGregorian 1911 5 1)
          (secondsToDiffTime 34123))
  , DbString "Hello, world!"
  , DbDate (UTCTime
           (fromGregorian 1921 5 1)
           (secondsToDiffTime 34123))
  , DbNumber 2
 ]

filterDbDate :: [DatabaseItem] -> [UTCTime]
filterDbDate d = foldr helper [] d
  where helper (DbDate a) b = a : b
        helper _ b = b

filterDbDate' :: [DatabaseItem] -> [UTCTime]
filterDbDate' d = foldr (\a b -> case a of
                            (DbDate a) -> a : b
                            (_) -> b) [] d

filterDbNumber :: [DatabaseItem] -> [Integer]
filterDbNumber = foldr (\a b -> case a of
                            (DbNumber a) -> a : b
                            (_) -> b) []

minDate = (UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0))

mostRecent :: [DatabaseItem] -> UTCTime
mostRecent = foldr (\a b -> if a > b then a else b) minDate  .  filterDbDate

sumDb :: [DatabaseItem] -> Integer
sumDb = foldr (\a b -> a + b) 0 . filterDbNumber

avgDb :: [DatabaseItem] -> Double
avgDb x =
      let numbers = filterDbNumber x
          lengthOfList = fromIntegral (length numbers)
          sumOfList = foldr (\a b -> a + b) 0 numbers
      in
        (fromIntegral sumOfList) / lengthOfList

