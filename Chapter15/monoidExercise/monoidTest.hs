import Data.Monoid
import Test.QuickCheck
import Control.Applicative

newtype First' a =
  First' { getFirst' :: Maybe a }
  deriving (Eq, Show)

instance  Monoid (First' a) where
  mempty = First' Nothing
  mappend (First' (Just a)) _ = (First' (Just a))
  mappend _ (First' (Just a))  = (First' (Just a))
  mappend _ _ = (First' Nothing)

-- type FirstMappend a = (First' a) -> (First' a) -> (First' a) -> Bool

prop_Associative :: (Eq m, Monoid m) => m -> m -> m -> Bool
prop_Associative a b c = (a <> b) <> c == a <> (b <> c)

prop_LeftIdentity :: (Eq m, Monoid m) => m -> Bool
prop_LeftIdentity a = (a <> mempty) == a

prop_RightIdentity :: (Eq m, Monoid m) => m -> Bool
prop_RightIdentity a = (mempty <> a) == a

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = First' <$> arbitrary

main :: IO ()
main = do
  quickCheck (prop_Associative :: (First' Int) -> (First' Int) -> (First' Int) -> Bool)
  quickCheck (prop_LeftIdentity :: (First' Int) -> Bool)
  quickCheck (prop_RightIdentity :: (First' Int) -> Bool)


