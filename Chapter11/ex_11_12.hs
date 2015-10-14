import Data.List
data Quantum =
    Yes
  | No
  | Both
  deriving (Eq, Show)
-- 3 + 3
quantSum1 :: Either Quantum Quantum
quantSum1 = Right Yes

quantSum2 :: Either Quantum Quantum
quantSum2 = Right No

quantSum3 :: Either Quantum Quantum
quantSum3 = Right Both

quantSum4 :: Either Quantum Quantum
quantSum4 = Left Yes

quantSum5 :: Either Quantum Quantum
quantSum5 = Left No

quantSum6 :: Either Quantum Quantum
quantSum6 = Left Both

-- 3 * 3
quantProd1 :: (Quantum, Quantum)
quantProd1 = (Yes, Yes)

quantProd2 :: (Quantum, Quantum)
quantProd2 = (Yes, No)

quantProd3 :: (Quantum, Quantum)
quantProd3 = (Yes, Both)

quantProd4 :: (Quantum, Quantum)
quantProd4 = (No, Yes)

quantProd5 :: (Quantum, Quantum)
quantProd5 = (No, No)

quantProd6 :: (Quantum, Quantum)
quantProd6 = (No, Both)

quantProd7 :: (Quantum, Quantum)
quantProd7 = (Both, Yes)

quantProd8 :: (Quantum, Quantum)
quantProd8 = (Both, No)

quantProd9 :: (Quantum, Quantum)
quantProd9 = (Both, Both)

-- 3 ^ 3

quantFlip1 :: Quantum -> Quantum
quantFlip1 Yes = Yes
quantFlip1 No = Yes
quantFlip1 Both = Yes

quantFlip2 :: Quantum -> Quantum
quantFlip2 Yes = Yes
quantFlip2 No = Yes
quantFlip2 Both = No

quantFlip3 :: Quantum -> Quantum
quantFlip3 Yes = Yes
quantFlip3 No = Yes
quantFlip3 Both = Both

quantFlip4 :: Quantum -> Quantum
quantFlip4 Yes = Yes
quantFlip4 No = No
quantFlip4 Both = Yes

quantFlip5 :: Quantum -> Quantum
quantFlip5 Yes = Yes
quantFlip5 No = Both
quantFlip5 Both = Yes

quantFlip6 :: Quantum -> Quantum
quantFlip6 Yes = No
quantFlip6 No = Yes
quantFlip6 Both = Yes

quantFlip7 :: Quantum -> Quantum
quantFlip7 Yes = Both
quantFlip7 No = Yes
quantFlip7 Both = Yes

quantFlip8 :: Quantum -> Quantum
quantFlip8 Yes = Both
quantFlip8 No = Yes
quantFlip8 Both = No

createQuantum :: [String] ->  String
createQuantum x =
  let
    possibilities = x
    combinations = [(x,y,z) | x <- possibilities, y <- possibilities, z <- possibilities]
  in
    snd $ foldr (\item acc -> (createAccumulator (fst acc) item (snd acc))) (0,[]) combinations
  where
    functionName x = "quantFlip" ++ show(x)
    header x =  functionName x ++ " :: Quantum -> Quantum\n"
    firstLine x y = genericLine x "Yes" y
    secondLine x y = genericLine x "No" y
    thirdLine x y = genericLine x "Both" y
    genericLine x y z = functionName x ++ " " ++  y ++ " =  " ++ z ++ "\n"
    createFunction x (a,b,c) = (header x) ++ (firstLine x a) ++ (secondLine x b) ++ (thirdLine x c) ++ "\n\n"
    createAccumulator x combo accString = let index = x + 1 in (index, accString ++ (createFunction index combo))


writeQuantumToFile :: [String] -> IO()
writeQuantumToFile x = writeFile "test.txt" (createQuantum x)

-- createQuantum :: [String] -> [String] -> String
-- createQuantum x y =
--   let patterMatchPossibilities = x
--       returnPossibilities = comb (length patternMatchPossibilities) y
--   in
--     snd $ foldr (\item acc -> (createAccumulator (fst acc) item (snd acc))) (0,[]) returnPossibilities
--   where
--     functionName x = "convert"
--     header x = functionName x ++ " :: Quantum -> Bool\n"
--     genericLine x y z = functionName x ++ " " ++ y ++ " = " ++ z ++ "\n"
--     createBody index xs =

test = ["Yes", "No", "Both"]

callThisFunction :: [String] -> [[String]]
callThisFunction x = nub $ createAllPermutations  $ superExpand x

createAllPermutations :: [[String]] -> [[String]]
createAllPermutations args = foldr (\item acc -> (permutations item) ++ acc) [[]] args

superExpand :: [String] -> [[String]]
superExpand args = foldr (\item acc -> (expand item) ++ acc) [[]]  $ (permutations args)

expand :: [String] -> [[String]]
expand [] = [[]]
expand l@(x:xs)
  | all (\x' -> x' == x) l = [l]
  | otherwise = let newList = ((takeFirstHalf l) ++ (replaceFirstEntry (takeSecondHalf l ) x)) in permutations l ++ expand newList

replaceFirstEntry :: (Eq a) => [a] -> a -> [a]
replaceFirstEntry [] _ = []
replaceFirstEntry (x:xs) a = a:xs

takeFirstHalf :: (Eq a) => [a] -> [a]
takeFirstHalf [] = []
takeFirstHalf l@(y:ys) = takeWhile (\x -> x == y) l

takeSecondHalf :: (Eq a) => [a] ->  [a]
takeSecondHalf []  = []
