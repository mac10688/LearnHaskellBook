e1 :: String -> String -> [(Char, Char, Char)]
e1 x y = [(i,j,k) | i <- x, j <- y, k <- x]

e1' :: String -> String -> [(Char, Char, Char)]
e1' x y = [(i,j,k) | i <- x, j <- y, k <- x, i == 'p']

e1'' :: [a] -> [b] -> [(a,b,a)]
e1'' nouns verbs = [(i,j,k) | i <- nouns, j <- verbs, k <- nouns]

seekritFunc :: String -> Int
seekritFunc x =
  div (sum (map length (words x)))
      (length (words x))

seekritFunc' :: Fractional a => String -> a
seekritFunc' x =
  let totalLetters = fromIntegral $ sum $ map length $ words x
      numberOfWords = fromIntegral $ length $ words x
  in totalLetters / numberOfWords

myOr :: [Bool] -> Bool
myOr = foldr (||) False

myAny :: (a -> Bool) -> [a] -> Bool
myAny _ [] = False
myAny f (x:xs) = (f x) || (myAny f xs)

myAny' :: (a -> Bool) -> [a] -> Bool
myAny' f xs = foldr (\item acc -> acc || (f item)) False xs

myAny'' :: (a -> Bool) -> [a] -> Bool
myAny'' f xs = foldr (\item acc -> acc || item) False $ map f xs

--myAny''' :: (a -> Bool) -> [a] -> Bool
--myAny''' = (foldr (||) False) . map
--myAny'' = \f -> \xs -> foldr (\item acc -> acc || (f item)) False xs
--myAny''' = \f -> foldr (\item acc -> acc || (f item)) False
--myAny'''' = flip foldr False . (\f -> (\item acc -> acc || (f item))

--myAny''''' = foldr . (\f -> \item -> \acc -> (||) acc (f item)) False


myElem :: Eq a => a -> [a] -> Bool
myElem a b = any (\x -> a == x) b

myElem' :: Eq a => a -> [a] -> Bool
myElem' = any . (==)

myReverse :: [a] -> [a]
myReverse = foldl (flip (:)) []

myMap :: (a -> b) -> [a] -> [b]
myMap f a = foldr (\item acc -> f item : acc) [] a

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f a = foldr (\item acc -> if f item then item:acc else acc) [] a

squish :: [[a]] -> [a]
squish = foldr (++) []

squishMap :: (a -> [b]) -> [a] -> [b]
squishMap f a = foldr (\item acc -> (f item) ++ acc) [] a

squishAgain :: [[a]] -> [a]
squishAgain = foldr (\item acc -> (squishMap (:[]) item) ++ acc ) []

myMaximumBy :: (a -> a -> Ordering) -> [a] -> a
myMaximumBy f (x:xs) = foldl (\item acc -> if (f item acc) == GT then item else acc ) x xs

myMinimumBy :: (a -> a -> Ordering) -> [a] -> a
myMinimumBy f (x:xs) = foldl (\item acc -> if (f item acc) == LT then item else acc) x xs
