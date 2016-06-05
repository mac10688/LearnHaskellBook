-- fizzBuzz :: Integer -> String
-- fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
--            | n `mod` 5 == 0 = "Fizz"
--            | n `mod` 3 == 0 = "Buzz"
--            | otherwise = show n

-- main :: IO()
-- main = mapM_ (putStrLn . fizzBuzz) [1..100]

import Control.Monad
import Control.Monad.Trans.State

fizzBuzz :: Integer -> String
fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
           | n `mod` 5 == 0 = "Fizz"
           | n `mod` 3 == 0 = "Buzz"
           | otherwise = show n

fizzBuzzList :: [Integer] -> [String]
fizzBuzzList list =
    execState (mapM_ addResult list) []

addResult :: Integer -> State [String] ()
addResult n = do
    xs <- get
    let result = fizzBuzz n
    put (result : xs)

fizzbuzzFromTo :: Integer -> Integer -> [String]
fizzbuzzFromTo from to = fizzBuzzList [from, (from - 1) .. to]

main :: IO ()
main = mapM_ putStrLn $ fizzbuzzFromTo 100 0 

-- import Control.Monad
-- import Control.Monad.State --Can't find this
-- import qualified Data.DList as DL -- Can't find this

-- fizzBuzz :: Integer -> String
-- fizzBuzz n | n `mod` 15 == 0 = "FizzBuzz"
--            | n `mod` 5 == 0 = "Fizz"
--            | n `mod` 3 == 0 = "Buzz"
--            | otherwise = show n

-- fizzbuzzList :: [Integer] -> [String]
-- fizzbuzzList list = 
--     let dlist = execState (mapM_ addResult list) DL.empty
--     in DL.apply dlist [] -- convert back to normal list

-- addResult :: Integer -> State (DL.DList String) ()
-- addResult n = do
--     xs <- get
--     let result = fizzBuzz n
--     -- snoc appends to the end, unlike
--     -- cons which adds to the front
--     put (DL.snoc xs result)

-- main :: IO ()
-- main = mapM_ putStrLn $ fizzbuzzList [1..100]

-- fizzbuzzList :: [Integer] -> DL.DList String
-- fizzbuzzList list = execState (mapM_ addResult list) DL.empty

-- addResult :: Integer -> State (DL.DList String) ()
-- addResult n = do
--     xs <- get
--     let result = fizzBuzz n
--     put (DL.snoc xs result)

-- main :: IO ()
-- main = mapM_ putStrLn $ fizzbuzzList [1..100]
