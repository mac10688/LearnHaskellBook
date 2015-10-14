brokenFact1 :: Integer -> Integer
brokenFact1 n = n * brokenFact1 (n -1)

factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial(n-1)

incTimes :: (Eq a, Num a) => a -> a -> a
incTimes 0 n = n
incTimes times n = n `seq` (incTimes (times - 1) (n + 1) )

applyTimes :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes 0 f b = b
applyTimes n f b = f (applyTimes(n-1) f b)

applyTimes' :: (Eq a, Num a) => a -> (b -> b) -> b -> b
applyTimes' 0 f b = b
applyTimes' n f b = f . applyTimes' (n-1) f $ b

incTimes' :: (Eq a, Num a) => a -> a -> a
incTimes' times n = applyTimes' times (+1) n
