myHead (x : _) = x

myHead' :: [a] -> Maybe a
myHead' [] = Nothing
myHead' (x:_) = Just x

myEnumFromTo :: (Enum a, Eq a) => a -> a -> [a]
myEnumFromTo x y
              | x == y = [x]
              | otherwise = x : myEnumFromTo (succ x) y

