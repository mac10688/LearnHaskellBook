module Lib
    ( someFunc
    ) where

import Data.Monoid

data Optional a = Nada | Only a deriving (Eq, Show)

instance Monoid a => Monoid (Optional a) where
  mempty = Nada
  mappend (Only a) (Only b) = Only (a `mappend` b)
  mappend Nada _ = Nada
  mappend _ Nada = Nada

someFunc :: IO ()
someFunc = putStrLn "someFunc"
