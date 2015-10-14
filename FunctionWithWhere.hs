module FunctionWithWhere where

printInc :: Integer -> IO()
printInc n = print plusTwo
  where plusTwo = n + 2

printInc2 :: Integer -> IO()
printInc2 n = let plusTwo = n + 2
              in print plusTwo

f1 :: Integer
f1 = x * 3 + y
  where x = 3
        y = 1000

f2 :: Integer
f2 = x * 5
  where y = 10
        x = 10 * 5 + y

f3 :: Float
f3 = z * x + y
  where x = 7
        y = negate x
        z = y / 10

waxOn :: Integer
waxOn = x * 5
  where z = 7
        y = z + 8
        x = y ^ 2

triple :: Integer -> Integer
triple x = 3 * x

waxOff :: Integer -> Integer
waxOff x = triple x
