import Data.Monoid

newtype Comp a = Comp {unComp :: a -> a }

-- instance Show a => Show (Comp a) where
  -- show x = unComp x

instance (Monoid a) => Monoid (Comp a) where
      mempty = Comp $ \_ -> mempty
      x `mappend` y = let
          x' = unComp x
          y' = unComp y
          in Comp $ \n -> ((x' n) <> (y' n))
