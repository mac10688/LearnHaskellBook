import Control.Monad
import System.Exit (exitSuccess)
import Data.Char

palindrome :: IO ()
palindrome = forever $ do
  line1 <- getLine
  -- case (line1 == reverse line1) of
  case (isPalindrome line1) of
    True -> putStrLn "It's a palindrome!"
    False -> exitSuccess


isPalindrome :: String -> Bool
isPalindrome xs =
              let
                onlyNumeric = map toLower  $ filter isAlpha xs
              in
                onlyNumeric == reverse onlyNumeric

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   | PersonInvalidUnknown String
                   deriving (Eq, Show)

mkPerson :: Name -> Age -> Either PersonInvalid Person
mkPerson name age
  | name /= "" && age > 0 = Right $ Person name age
  | name == "" = Left NameEmpty
  | not (age > 0) = Left AgeTooLow
  | otherwise = Left $ PersonInvalidUnknown $
                       "Name was: " ++ show name ++
                       " Age was: " ++ show age

gimmePerson :: IO ()
gimmePerson = do
  putStrLn "Give me your name."
  name <- getLine
  putStrLn "Give me your age."
  age <- getLine
  case mkPerson name (read age :: Integer ) of
    (Right person) -> putStrLn $ "Yay! Successfully got a person:" ++ (show person)
    (Left reason) -> putStrLn $ "An error occured: " ++ (show reason)


