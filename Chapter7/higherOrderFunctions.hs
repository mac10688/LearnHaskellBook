dodgy x y = x + y * 10
oneIsOne = dodgy 1
oneIsTwo = (flip dodgy) 2

myAbs :: Integer -> Integer
myAbs x
  | x < 0 = (-x)
  | otherwise = x

boodNa :: Integer -> String
boodNa x
  | x < 135 = "too low"
  | x > 145 = "too high"
  | otherwise = "just right"

isRight :: (Num a, Eq a) => a -> a -> a -> String
isRight a b c
  | a^2 + b^2 == c^2 = "RIGHT ON"
  | otherwise = "not right"

dogYrs :: (Num a, Ord a) => a -> a
dogYrs x
  | x <= 0 = 0
  | x <= 1 = x * 15
  | x <= 2 = x * 12
  | x <= 4 = x * 8
  | otherwise = x * 6

avgGrade :: (Fractional a, Ord a) => a -> Char
avgGrade x
  | y >= 0.9 = 'A'
  | y >= 0.7 = 'C'
  | y >= 0.8 = 'B'
  | y >= 0.6 = 'D'
  | otherwise = 'F'
  where y = x / 100

pal xs
  | xs == reverse xs = True
  | otherwise = False

members x
  | x < 0 = -1
  | x == 0 = 0
  | x > 0 = 1
