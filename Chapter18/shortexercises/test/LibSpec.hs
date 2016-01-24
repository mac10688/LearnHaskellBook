{-# OPTIONS_GHC -fno-warn-orphans #-}

module LibSpec where

import Test.Hspec
import Test.QuickCheck
import Control.Monad (join)
import Control.Applicative ((*>))
import Data.Monoid((<>))

import Lib 

bind :: Monad m => (a -> m b) -> m a -> m b
bind f a = join $ fmap f a

sequencing :: IO ()
sequencing = do
    putStrLn "blah"
    putStrLn "another thing"

sequencing' :: IO ()
sequencing' =
    putStrLn "blah" >>
    putStrLn "another thing"

sequencing'' :: IO ()
sequencing'' =
    putStrLn "blah" *>
    putStrLn "another thing"

binding :: IO ()
binding = do
    name <- getLine
    putStrLn name

binding' :: IO ()
binding' =
    getLine >>= putStrLn

bindingAndSequencing :: IO ()
bindingAndSequencing = do
    putStrLn "name pls"
    name <- getLine
    putStrLn ("y helo thar: " ++ name)

bindingAndSequencing' :: IO ()
bindingAndSequencing' =
    putStrLn "name pls:" >>
    getLine >>=
    \name -> putStrLn ("y helo thar: " ++ name)

twoBinds :: IO ()
twoBinds = do
    putStrLn "name pls:"
    name <- getLine
    putStrLn "age pls:"
    age <- getLine
    putStrLn ("y helo thar: "
              ++ name ++ " who is: "
              ++ age ++ " years old.")

twoBinds' :: IO ()
twoBinds' =
    putStrLn "name pls:" >>
    getLine >>=
    \name ->
    putStrLn "age pls:" >>
    getLine >>=
    \age ->
    putStrLn ("y helo thar: "
              ++ name ++ " who is: "
              ++ age ++ " years old.")

twoBinds'' :: IO ()
twoBinds'' =
    putStrLn "name pls:" >>
    getLine >>=
     (\name ->
      putStrLn "age pls:" >>
      getLine >>=
      (\age ->
       putStrLn ("y helo thar: "
                 ++ name ++ " who is: "
                 ++ age ++ " years old.")))

twiceWhenEven :: [Integer] -> [Integer]
twiceWhenEven xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else [x*x]

twiceWhenEven' :: [Integer] -> [Integer]
twiceWhenEven' xs = do
    x <- xs
    if even x
        then [x*x, x*x]
        else []

data Cow = Cow {
    name :: String
    , age :: Int
    , weight :: Int
} deriving (Eq, Show)

noEmpty :: String -> Maybe String
noEmpty "" = Nothing
noEmpty str = Just str

noNegative :: Int -> Maybe Int
noNegative n | n >= 0 = Just n
             | otherwise = Nothing

-- if Cow's name is Bess, must be under 500
weightCheck :: Cow -> Maybe Cow
weightCheck c =
    let w = weight c
        n = name c
    in if n == "Bess" && w > 499
        then Nothing
        else Just c

mkSphericalCow :: String -> Int -> Int -> Maybe Cow
mkSphericalCow name' age' weight' =
    case noEmpty name' of
        Nothing -> Nothing
        Just nammy ->
            case noNegative age' of
                Nothing -> Nothing
                Just agey ->
                    case noNegative weight' of
                        Nothing -> Nothing
                        Just weighty ->
                            weightCheck (Cow nammy agey weighty)

-- Do syntax isn't just for IO

mkSphericalCow' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow' name' age' weight' = do
    nammy <- noEmpty name'
    agey <- noNegative age'
    weighty <- noNegative weight'
    weightCheck (Cow nammy agey weighty)

mkSphericalCow'' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow'' name' age' weight' =
    noEmpty name' >>=
    \ nammy ->
      noNegative age' >>=
      \ agey ->
        noNegative weight' >>=
        \ weighty ->
        weightCheck (Cow nammy agey weighty)

f :: Maybe Integer
f = Just 1

g :: Maybe String
g = Just "1"

h :: Maybe Integer
h = Just 10191

zed :: a -> b -> c -> (a, b, c)
zed = (,,)

doSomething = do
    a <- f
    b <- g
    c <- h
    return (zed a b c)

zed' :: Monad m => a -> b -> c -> m (a, b, c)
zed' a b c = return (a, b, c)

doSomething' = do
    a <- f
    b <- g
    c <- h
    zed' a b c

mkSphericalCow''' :: String -> Int -> Int -> Maybe Cow
mkSphericalCow''' name' age' weight' =
    noEmpty name' >>=
    \ nammy ->
      noNegative age' >>=
      \ agey ->
        noNegative weight' >>=
        \ weighty ->
          weightCheck (Cow nammy agey weighty)

data Sum a b =
    First a
  | Second b
  deriving (Eq, Show)

instance Functor (Sum a) where
    fmap _ (First a) = First a
    fmap f (Second b) = Second (f b)

instance Monoid a => Applicative (Sum a) where
    pure x = Second x
    Second f <*> Second b' = Second (f b')
    First a <*> First a' = First (a <> a')
    Second _ <*> First a = First a
    First x <*> _ = First x

instance Monoid a => Monad (Sum a) where
    return = pure
    (First a) >>= _ = First a
    (Second b) >>= f = (f b)
