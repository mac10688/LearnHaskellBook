module Print2 where

main :: IO()
main = do
  putStrLn "Count to four for me:"
  putStr "one, two"
  putStr ", thre, and"
  putStrLn "four!"
