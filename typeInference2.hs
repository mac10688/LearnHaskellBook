module TypeInference2 where

--f x y = x + y + 3

-- x =5
-- y=x+5
-- f=4/y

bigNum = (^) 5 $ 10
wahoo = bigNum ^ 0

-- x=print
-- y = print "wohoo!"
-- z = x "hello world"

-- a = (+)
-- b = 5
-- c = a 10
-- d = c 200

-- a = 12 + b
-- b = 10000 * c
-- c = 0

functionH :: [[a]] -> [a]
functionH [] = []
functionH (x:_) = x

functionC :: (Ord a) => a -> a -> Bool
functionC x y = if (x>y) then True else False

functionS :: (a,b) -> b
functionS (_,y) = y

i :: a -> a
i a = a

c :: a -> b -> a
c a _ = a

r :: [a] -> [a]
r x = tail x

co :: (b -> c) -> (a -> b) -> (a -> c)
co x y = x . y

a :: (a -> c) -> a -> a
a _ y = y

a' :: (a -> b) -> a -> b
a' x y = x y

