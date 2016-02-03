module QualityControlTests where

import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes

import QualityControl

main = do
    let trigger = undefined :: TI (Int, Int, [Int])
    quickBatch (traversable trigger)
