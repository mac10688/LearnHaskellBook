bindExp :: Integer -> String
bindExp x = let x = 10; y = 5 in "x: " ++ show x ++ " y: " ++ show y

first x y z = x * y * z
second x y = \ z -> x * y * z
third x = \y -> \z -> x * y *z
fourth = \x -> \y -> \z -> x * y * z

addOneIfOdd n = case odd n of
  True -> f n
  False -> n
  where f = (\n -> n + 1)

addFive = \x -> \y -> (if x > y then y else x) + 5

mflip f x y = f y x

isItTwo :: Integer -> Bool
-- isItTwo _ = False
isItTwo 2 = True
