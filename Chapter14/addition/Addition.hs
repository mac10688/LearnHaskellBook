module Addition where

import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = hspec $ do
  describe "Addition" $ do
   it "15 divided by 3 is 5" $ do
      dividedBy 15 3 `shouldBe` (5,0)
   it "22 divided by 5 is 4 remainder 2" $ do
      dividedBy 22 5 `shouldBe` (4,2)


dividedBy :: Integral a => a -> a -> (a,a)
dividedBy num denom = go num denom 0
  where go n d count
          |n < d = (count, n)
          |otherwise = go (n -d) d (count + 1)

multiplyTests :: IO ()
multiplyTests = hspec $ do
  describe "Multiply" $ do
    it "100 * 100 = 10,000" $ do
      g 100 100 `shouldBe` 10000
    it "0 * 1 = 0" $ do
      g 0 1 `shouldBe` 0
    it "1 * 0 = 0" $ do
      g 1 0 `shouldBe` 0
    it "1 * -5 = -5" $ do
      g 1 (-5) `shouldBe` (-5)
    it "x + 1 is always greater than x" $ do
      property $ \x -> x + 1> (x :: Int)

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

trivialInt :: Gen Int
trivialInt = return 1

oneThroughThree :: Gen Int
oneThroughThree = elements [1, 2, 3]

genBool :: Gen Bool
genBool = choose (False, True)

genBool' :: Gen Bool
genBool' = elements[False, True]

genOrdering :: Gen Ordering
genOrdering = elements [LT, EQ, GT]

genChar :: Gen Char
genChar = elements ['a'..'z']

genTuple :: (Arbitrary a, Arbitrary b) => Gen (a, b)
genTuple = do
  a <- arbitrary
  b <- arbitrary
  return (a, b)

genThreeple :: (Arbitrary a, Arbitrary b, Arbitrary c) => Gen (a, b, c)
genThreeple = do
  a <- arbitrary
  b <- arbitrary
  c <- arbitrary
  return (a, b, c)

genEither :: (Arbitrary a, Arbitrary b) => Gen (Either a b)
genEither = do
  a <- arbitrary
  b <- arbitrary
  elements [Left a, Right b]

-- equal probability
genMaybe :: Arbitrary a => Gen (Maybe a)
genMaybe = do
  a <- arbitrary
  elements [Nothing, Just a]

-- What QuickCheck actually does
-- so you get more Just values
genMaybe' :: Arbitrary a => Gen (Maybe a)
genMaybe' = do
  a <- arbitrary
  frequency [ (1, return Nothing)
            , (3, return (Just a))]

-- frequence :: [(Int, Gen a)] -> Gen a

prop_additionGreater :: Int -> Bool
prop_additionGreater x = x + 0 > x

runQc :: IO ()
runQc = quickCheck prop_additionGreater
