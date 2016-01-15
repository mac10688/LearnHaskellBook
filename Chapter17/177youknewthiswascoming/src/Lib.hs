-- | A library to do stuff.
module Lib where

data Bull =
        Fools
      | Twoo
      deriving (Eq, Show)

instance Monoid Bull where
    mempty = Fools
    mappend _ _ = Fools
