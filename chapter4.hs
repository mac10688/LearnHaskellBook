isPalindrome :: (Eq a) => [a] -> Bool
isPalindrome x = reverse x == x

myAbs :: Integer -> Integer
myAbs x = if x < 0 then negate x else x

f :: (a,b) -> (c,d) -> ((b,d), (a,c))
f x y = ((snd x, snd y), (fst x, fst y))

x = (+)
g xs = w `x` 1
  where w = length xs

h = \x -> x

i = \(x:xs) -> x

j(a,b) = a

