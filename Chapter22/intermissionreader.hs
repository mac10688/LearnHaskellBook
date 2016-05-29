import Data.Char
import Control.Applicative (liftA2)

cap :: [Char] -> [Char]
cap xs = map toUpper xs

rev :: [Char] -> [Char]
rev xs = reverse xs

composed :: [Char] -> [Char]
composed = cap . rev

fmapped :: [Char] -> [Char]
fmapped = fmap cap rev

tupled :: [Char] -> ([Char], [Char])
tupled = liftA2 (,) cap rev

tupled' :: [Char] -> ([Char], [Char])
tupled' = do
    a <- cap
    b <- rev
    return (a, b)

tupled'' :: [Char] -> ([Char], [Char])
tupled'' = cap >>= \a ->
           rev >>= \b ->
           return (a, b)

newtype Reader r a = 
    Reader { runReader :: r -> a }

ask :: Reader a a
ask = Reader id
