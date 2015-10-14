f1 :: Int -> Int -> Bool
f1 x y = (quot x y) *y + (rem x y) == x

f2 :: Int -> Int -> Bool
f2 x y = (div x y) * y + (mod x y) == x

f3 :: Int -> Int -> Bool
