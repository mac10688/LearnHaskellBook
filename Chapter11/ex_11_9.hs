module Jammin where

import Data.List

data Fruit =
    Peach
  | Plum
  | Apple
  | Blackberry
  deriving (Eq, Show, Ord)

data JamJars =
  JamJars {
      fruit :: Fruit
    , quantity :: Int
  } deriving (Eq, Show, Ord)

row1 = JamJars Peach 10
row2 = JamJars Plum 2
row3 = JamJars Apple 5
row4 = JamJars Blackberry 10
row5 = JamJars Peach 10
row6 = JamJars Plum 2

allJam = [row1, row2, row3, row4, row5, row6]

totalNumber :: Int
totalNumber = foldr (+) 0 $ map quantity allJam

mostRow :: JamJars
mostRow = maximumBy (\a b -> compare (quantity a) (quantity b)) allJam

sortJams :: [JamJars]
sortJams = sortBy (\a b -> compare (fruit a) (fruit b)) allJam

groupJams :: [[JamJars]]
groupJams = groupBy (\a b -> fruit a == fruit b) sortJams
