import Data.Char
import Data.List

data Weekday =
    Monday
  | Tuesday
  | Wednesday
  | Thursday
  | Friday

--Weekday is a type with five data constructors

-- f Friday = "Miller Time"

-- f :: Weekday -> String

-- types defined with the data keyword must begin with a capital letter.

g xs = xs !! (length xs - 1)

--delivers the final element of xs

f :: Show a => (a,b) -> IO (a,b)
f t@(a,_) = do
    print a
    return t

doubleUp :: [a] -> [a]
doubleUp [] = []
doubleUp xs@(x:_) = x : xs

isSubsequenceOf :: (Eq a) => [a] -> [a] -> Bool
isSubsequenceOf [] _ = True
isSubsequenceOf _ [] = False
isSubsequenceOf t@(x:xs) (y:ys) = if x == y then isSubsequenceOf xs ys else isSubsequenceOf t ys

-- capitalizeWords :: String -> [(String, String)]
-- capitalizeWords = et t@(x:xs) = item in (t, (toUpper x): xs):acc ) [] . words

capitalizeWord :: String -> String
capitalizeWord (x:xs) = (toUpper x) : xs

capitalizeParagraph :: String -> String
capitalizeParagraph = concatMap capitalizeFirstLetter . splitSentences

splitSentences :: String -> [String]
splitSentences = splitOn '.'

splitOn :: Char ->  String -> [String]
splitOn charToSplitOn string = foldr (\item acc -> let t@(x:xs) = acc in if item == charToSplitOn then [item]:t else (item:x):xs) [[]] string

capitalizeFirstLetter :: String -> String
capitalizeFirstLetter [] = []
capitalizeFirstLetter (x:xs) = if isAlpha x then (toUpper x):xs else x:capitalizeFirstLetter xs

data PhoneButton = PhoneButton {
      alphaUpperCase :: Char
    , alphaLowerCase :: Char
    , button :: Char
    , presses :: Int
} deriving (Show, Eq)

data DaPhone = DaPhone [PhoneButton] deriving (Show, Eq)

makePhone :: DaPhone
makePhone = DaPhone [
            (PhoneButton 'A' 'a' '2' 1),
            (PhoneButton 'B' 'b' '2' 2),
            (PhoneButton 'C' 'c' '2' 3),
            (PhoneButton 'D' 'd' '3' 1),
            (PhoneButton 'E' 'e' '3' 2),
            (PhoneButton 'F' 'f' '3' 3),
            (PhoneButton 'G' 'g' '4' 1),
            (PhoneButton 'H' 'h' '4' 2),
            (PhoneButton 'I' 'i' '4' 3),
            (PhoneButton 'J' 'j' '5' 1),
            (PhoneButton 'K' 'k' '5' 2),
            (PhoneButton 'L' 'l' '5' 3),
            (PhoneButton 'M' 'm' '6' 1),
            (PhoneButton 'N' 'n' '6' 2),
            (PhoneButton 'O' 'o' '6' 3),
            (PhoneButton 'P' 'p' '7' 1),
            (PhoneButton 'Q' 'q' '7' 2),
            (PhoneButton 'R' 'r' '7' 3),
            (PhoneButton 'S' 's' '7' 4),
            (PhoneButton 'T' 't' '8' 1),
            (PhoneButton 'U' 'u' '8' 2),
            (PhoneButton 'V' 'v' '8' 3),
            (PhoneButton 'W' 'w' '9' 1),
            (PhoneButton 'X' 'x' '9' 2),
            (PhoneButton 'Y' 'y' '9' 3),
            (PhoneButton 'Z' 'z' '9' 4),
            (PhoneButton ' ' ' ' '0' 1),
            (PhoneButton '1' '1' '1' 1),
            (PhoneButton '2' '2' '2' 2),
            (PhoneButton '3' '3' '3' 3),
            (PhoneButton '4' '4' '4' 4),
            (PhoneButton '5' '5' '5' 5),
            (PhoneButton '6' '6' '6' 6),
            (PhoneButton '7' '7' '7' 7),
            (PhoneButton '8' '8' '8' 8),
            (PhoneButton '9' '9' '9' 9),
            (PhoneButton '*' '*' '*' 1)
            ]


convo :: [String]
convo = ["Wanna play 20 questions",
         "Ya",
         "U 1st haha",
         "Lol ok. Have u ever tasted alcohol lol",
         "Lol ya",
         "Wow ur cool haha. Ur turn",
         "Ok. Do u think I am pretty Lol",
         "Lol ya",
         "Haha thanks just making sure rofl ur turn"]

type Button = Char
type Presses = Int

cellPhonesDead :: DaPhone -> String -> [(Button, Presses)]
cellPhonesDead d x = foldr (\item acc ->  (findDigitForChar d item) : acc) [] x

findDigitForChar :: DaPhone -> Char ->  (Button, Presses)
findDigitForChar (DaPhone d) c =
                        case find (\ PhoneButton {alphaUpperCase=c'} -> c' == (toUpper c) ) d of
                              Just (PhoneButton { button=d, presses=p  }) -> ( d, p)
                              Nothing -> error "That shouldn't have happened, I'm not coped to deal with this yet!"

fingerTaps :: [(Button, Presses)] -> Presses
fingerTaps = foldr (\(_,presses) acc -> presses + acc ) 0

popularest :: String -> Char
popularest =
            head
          . last
          . sortBy (\l r -> (length l) `compare` (length r))
          . group
          . sort
          . map (\x -> toLower x)
          . filter (\x -> x /= ' ')

costMostPopular :: String -> Presses
costMostPopular s = let popularLetter = popularest s in
                snd (findDigitForChar makePhone popularLetter) *(length $ filter (\x -> x == popularLetter ) s)


coolestLtr :: [String] -> Char
coolestLtr = popularest . concat

coolestWord :: [String] -> String
coolestWord = head . last . sortBy (\l r -> (length l) `compare` (length r)) . group . sort . words . concat
