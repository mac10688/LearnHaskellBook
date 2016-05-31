module RaderPractice where

import Control.Applicative
import Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b
-- lookup _ [] = None
-- lookup a ((x,y):xs) = if a == x then Just y else lookup a xs

-- zip x and y using 3 as the lookup key
xs :: Maybe Integer
xs = let zipped = zip x y in
     lookup 3 zipped

-- zip y and z using 6 as the lookup key
ys :: Maybe Integer
ys = let zipped = zip y z in
     lookup 6 zipped

-- it's also nice to have one that
-- will return Nothing, like this one
-- zip x and y using 4 as the lookup key
zs :: Maybe Integer
zs = lookup 4 $ zip x y

-- -- now zip x and z using a variable lookup key
z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = xs >>= (\x -> sequenceA (x, ys))

x2 :: Maybe (Integer, Integer)
x2 = ys >>= (\x -> sequenceA (x, zs))

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = (z' n, z' n)

-- -- x1
-- -- Just (6,9)

-- -- x2
-- -- Nothing

-- --x3 3
-- --(Just 9, Just 9)

-- -- that first argument is a function
-- -- in this case, we want it to be addition
-- -- summed is just uncurry with addition as
-- -- the first argument
-- uncurry :: (a -> b -> c) -> (a, b) -> c
-- uncurry = undefined

summed :: Num c => (c, c) -> c
summed xy = uncurry (+) xy

-- use &&, >3, <8
bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

-- fromMaybe :: a -> Maybe a -> a
-- fromMaybe = undefined

-- -- fromMaybe 0 xs
-- -- 6

-- -- fromMaybe 0 zs
-- -- 0

main :: IO()
main = do
    print $ sequenceA [Just 3, Just 2, Just 1]
    print $ sequenceA [x, y]
    print $ sequenceA [xs, ys]
    print $ summed <$> ((,) <$> xs <*> ys)
    print $ fmap summed ((,) <$> xs <*> zs)
    print $ bolt 7
    print $ fmap bolt z
    print $ sequenceA [(>3), (<8), even] 7
    print $ sequA' 4
    print $ sequA' $ fromMaybe 0 s'
    print $ bolt $ fromMaybe 0 ys
    print $ applyBoltToz 1

-- -- sequenceA :: (Applicative f, Traversable t) => t (f a) -> f (t a)
-- -- so in this:
-- -- sequence [(>3), (<8), even] 7
-- -- f ~ (->) a and t ~ []

-- -- main
-- -- Just [3,2,1]
-- -- [[1,4],[1,5],[1,6],[2,4],[2,5],[2,6],[3,4],[3,5],[3,6]]
-- -- Just [6,9]
-- -- Just 15
-- -- Nothing
-- -- True
-- -- [True,False,False]

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

-- 1. fold the boolean conjunction operator over the list of results of
-- sequA (applied to some value)

sequA' :: Integer -> Bool
sequA' = foldr (&&) True . sequA

-- 2. apply sequA to s'; you'll need fromMaybe

applySequA = sequA' $ fromMaybe 0 s' 

-- 3. apply bolt to ys; you'll need fromMaybe

applyBoltToys = bolt $ fromMaybe 0 ys

-- 4. apply bolt to z'

applyBoltToz = bolt . fromMaybe 0 . z'
