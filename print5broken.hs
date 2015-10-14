module Print5Broken where

printSecond :: IO()
printSecond = do
  putStrLn greeting

greeting :: String
greeting = "Yarrrr"

main :: IO()
main = do
  putStrLn greeting
  printSecond
