module WordNumberTest where

import Test.Hspec
import qualified Test.QuickCheck as Q
import WordNumber (digitToWord, digits, wordNumber)
import Data.List
import Data.Char

main :: IO ()
main = hspec $ do
  describe "digitToWord does what we want" $ do
    it "returns zero for 0" $ do
      digitToWord 0 `shouldBe` "zero"
    it "returns one for 1" $ do
      digitToWord 1 `shouldBe` "one"

  describe "digits does what we want" $ do
    it "returns [1] for 1" $ do
      digits 1 `shouldBe` [1]
    it "returns [1, 0, 0] for 100" $ do
      digits 100 `shouldBe` [1, 0, 0]

  describe "wordNumber does what we want" $ do
    it "returns one-zero-zero for 1" $ do
      wordNumber 100 `shouldBe` "one-zero-zero"
    it "returns nine-zero-zero-one for 9001" $ do
      wordNumber 9001 `shouldBe` "nine-zero-zero-one"

half :: (Fractional a, Eq a) => a -> a
half x = x / 2

halfIdentity :: (Fractional a, Eq a) => a -> a
halfIdentity = (*2) . half

prop_half :: Double -> Bool
prop_half x = halfIdentity x == x

square :: (Num a, Eq a) => a -> a
square x = x * x

squareIdentity :: (Floating a, Eq a) => a -> a
squareIdentity = square . sqrt

prop_square :: Float -> Bool
prop_square x = squareIdentity x == x

plusAssociative :: Integer -> Integer -> Integer -> Bool
plusAssociative x y z = x + ( y + z ) == (x + y) + z

plusCommutative :: Integer -> Integer -> Bool
plusCommutative x y = x + y == y + x

multiAssociative :: Integer -> Integer -> Integer -> Bool
multiAssociative x y z = x * ( y * z ) == (x * y) * z

multiCommutative :: Integer -> Integer -> Bool
multiCommutative x y = x * y == y * x

quoteTest :: Integer -> Integer -> Bool
quoteTest x y = (quot x y)*y + (rem x y) == x

divTest :: Integer -> Integer -> Bool
divTest x y = (div x y)*y + (mod x y) == x

isPowerAssoc :: Integer -> Integer -> Integer -> Bool
isPowerAssoc x y z = x ^ (y ^ z) == (x ^ y) ^ z

isPowerCommutative :: Integer -> Integer -> Bool
isPowerCommutative x y = x ^ y == y ^ x

isReverseReverseOriginal ::  [Char] -> Bool
isReverseReverseOriginal x = reverse ( reverse x) == x

whatDoesDollarSignDo :: (Eq b) => (a -> b) -> a -> Bool
whatDoesDollarSignDo f a = (f a) == (f $ a)

f :: Int -> [Int] -> Bool
f n xs = length (take n xs) == n

f' :: Int -> Bool
f' x = (read (show x)) == x

twice f = f . f
fourTimes = twice . twice

capitalizeWord (x:xs) = (toUpper x):xs
capitalizeWord [] = []

f'' x = (capitalizeWord x == twice capitalizeWord x)
        && (twice capitalizeWord x == fourTimes capitalizeWord x)

f''' :: [Int] -> Bool
f''' x = (sort x == twice sort x)
         && (twice sort x  == fourTimes sort x)

runQc :: IO()
runQc = do
  -- Q.quickCheck prop_half)
  -- Q.quickCheck prop_square
  -- Q.quickCheck plusAssociative
  -- Q.quickCheck plusCommutative
  -- Q.quickCheck multiAssociative
  -- Q.quickCheck multiCommutative
  -- Q.quickCheck quoteTest
  -- Q.quickCheck divTest
  -- Q.quickCheck isPowerCommutative
  -- Q.quickCheck isPowerAssoc
  -- Q.quickCheck isReverseReverseOriginal
  -- Q.quickCheck $ whatDoesDollarSignDo (\x -> (x :: Int))
  -- Q.quickCheck f
  -- Q.quickCheck f'
  Q.quickCheck f''
  Q.quickCheck f'''

data Fool = Fulse | Frue deriving (Eq, Show)

fiftyPercent = Q.elements [Fulse, Frue]

twoThirdPercent = Q.elements [Fulse, Fulse, Frue]
