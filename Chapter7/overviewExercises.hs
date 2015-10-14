-- 1. d
-- 2. b
-- 3. d
-- 4. b
-- 5. a

--1.
tensDigit :: Integral a => a -> a
tensDigit x = d
  where xLast = x `div` 10
        d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' =  snd. flip divMod 10 . fst . flip divMod 10

hunsD :: Integral a => a -> a
hunsD = snd . flip divMod 10 . fst . flip divMod 10 . fst . flip divMod 10

foldBool1 :: a -> a -> Bool -> a
foldBool1 x y bool
 | bool = x
 | otherwise = y

foldBool2 :: a -> a -> Bool -> a
foldBool2 x y bool = case bool of
  True -> x
  otherwise -> y

g :: (a -> b) -> (a, c) -> (b, c)
g f (x, y) = (f x , y)


