module Main where

-- Gonna be a CAF this time.
incdInts :: [Integer] -> [Integer]
incdInts = map (+1)

main :: IO ()
main = do
    print (incdInts [1..] !! 1000)
