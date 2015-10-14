cattyConny :: String -> String -> String
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny

appedCatty = cattyConny "woops"
frappe = flippy "haha"


-- "woops mrow 2 mrow haha"
-- "woops mrow blue mrow haha"
-- "pink mrow haha mrow green mrow woops mrow blue"
-- "are mrow Pugs mrow awesome"

dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count
          | n < d = (count, n)
          | otherwise = go (n-d) d (count + 1)

--dividedBy 15 2
--go 15 2 0
--go 13 2 1
--go 11 2 3
--go 9  2 4
--go 7  2 5
--go 5  2 6
--go 3  2 7
--go 1  2 8
--(8,1)

f :: (Eq a, Num a) => a -> a
f x
  | x == 0 = 0
  | otherwise = x + f (x-1)

f' :: (Eq a, Num a) => a -> a
f' x = go x x
  where go cnt acc
         | cnt == 0 = acc
         | otherwise = cnt `seq` acc `seq` go (cnt-1) (acc + (cnt-1))

g :: (Ord a, Num a) => a -> a -> a
g x y
  | x == 0 = 0
  | y == 0 = 0
  | otherwise =
        let x' = if x < 0 then (-x) else x
            y' = if y < 0 then (-y) else y
            answer = go x' y'
        in
        if (shouldBeNegative x y) then (-answer) else answer
  where go x 0 = 0
        go x y = x + go x (y-1)

        shouldBeNegative x y
                          | x < 0 && y >0 = True
                          | x > 0 && y < 0 = True
                          | otherwise = False

g' :: (Eq a, Num a) => a -> a -> a
g' x y
  | x == 0 = 0
  | y == 0 = 0
  | otherwise =
    let x' = if isNegative x then (-x) else x
        y' = if isNegative y then (-y) else y
        answer = go x' y'
    in
    if (isProductNegative x y) then (-answer) else answer
  where go x 0 = 0
        go x y = x + go x (y-1)

        isNegative x = signum x == -1
        isPositive x = signum x == 1 --Readability
        isProductNegative x y
                          | isNegative x && isPositive y = True
                          | isPositive x && isNegative y = True
                          | otherwise = False

mc91 :: Integer -> Integer
mc91 x
  | x > 100 = x - 10
  | otherwise = 91
