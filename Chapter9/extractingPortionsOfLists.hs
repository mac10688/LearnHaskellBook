import StringSplitter
myWords :: [Char] -> [[Char]]
myWords [] = []
myWords x = (getFirstWord x) : (myWords (getRestOfSentence x))
          where
          getFirstWord = takeWhile (/= ' ') . dropWhile (== ' ')
          getRestOfSentence = dropWhile (== ' ') . dropWhile (/= ' ')

myWords' :: String -> [String]
myWords' = splitString ' '
