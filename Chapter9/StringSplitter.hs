module StringSplitter where

splitString :: Char -> String -> [String]
splitString _ [] = []
splitString split x = firstSection x : splitString split (secondSection x)
  where
    firstSection = takeWhile (/= split).dropWhile(== split)
    secondSection = dropWhile(== split). dropWhile (/= split)
