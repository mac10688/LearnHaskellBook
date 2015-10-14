import Data.Char

e2 :: String -> String
e2 = filter isUpper

e3 :: String -> String
e3 [] = []
e3 (x:xs) = toUpper x : xs

e4 :: String -> String
e4 [] = []
e4 (x:xs) = toUpper x : e4 xs

e5 :: String -> Char
e5 [] = ' '
e5 x = toUpper $ head x

e6 :: String -> Char
e6 = toUpper . head

myOr :: [Bool] -> Bool
myOr [] = True
myOr (x:xs) = if x == True then True else myOr xs

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = if f x then True else myAny f xs

myElem :: Eq a => a -> [a] -> Bool
myElem _ [] = False
myElem x (y:ys) = if x == y then True else myElem x ys

myElem' :: Eq a => a -> [a] -> Bool
myElem' x xs = myAny (\z -> z == x) xs

myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

squish :: [[a]] -> [a]
squish [] = []
squish (x:xs) = x ++ squish xs

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap _ [] = []
squishMap f (x:xs) = (f x) ++ (squishMap f xs)

squishAgain :: [[a]] -> [a]
squishAgain xs = squishMap id xs

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:[]) = x
myMaximumBy f (x:y:xs) = if f x y == GT then x else myMaximumBy f (y:xs)

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:[]) = x
myMinimumBy f (x:y:xs) = if f x y == LT then x else myMinimumBy f (y:xs)

myMaximum :: (Ord a) => [a] -> a
myMaximum (x:[]) = x
myMaximum (x:y:xs) = if x > y then x else myMaximum (y:xs)

myMinimum :: (Ord a) => [a] -> a
myMinimum (x:[]) = x
myMinimum (x:y:xs) = if x < y then x else myMinimum (y:xs)
