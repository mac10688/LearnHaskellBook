module CriterionExample2 where

import Criterion.Main

myList :: [Int]
myList = [1..9999]

myList' :: [Int]
myList' = (undefined : [2..9999])

myList'' :: [Int]
myList'' = (undefined : undefined)

myList''' :: [Int]
myList''' = undefined

main :: IO ()
main = defaultMain 
      [ bench "map list 9999" $ whnf (map (+1)) myList
      , bench "map list 9999" $ whnf (map (+1)) myList'
      , bench "map list 9999" $ whnf (map (+1)) myList''
      , bench "map list 9999" $ nf (map (+1)) myList
      , bench "map list 9999" $ whnf (map (+1)) myList''']

