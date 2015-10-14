ifEvenAdd2 :: Integer -> Maybe Integer
ifEvenAdd2 n = if even n then Just (n+2) else Nothing

type Name = String
type Age = Integer

data Person = Person Name Age deriving Show

-- mkPerson :: Name -> Age -> Maybe Person
-- mkPerson name age
--   | name /= "" && age > 0 = Just $ Person name age
--   | otherwise = Nothing

type ValidatePerson a = Either [PersonInvalid] a

data PersonInvalid = NameEmpty
                   | AgeTooLow
                   deriving (Eq, Show)


-- mkPerson :: Name -> Age -> Either PersonInvalid Person
-- mkPerson name age
--   | name /= "" && age > 0 = Right $ Person name age
--   | name == "" = Left NameEmpty
--   | otherwise = Left AgeTooLow

ageOkay :: Age -> Either [PersonInvalid] Age
ageOkay age = case age > 0 of
  True -> Right age
  False -> Left [AgeTooLow]

nameOkay :: Name -> Either [PersonInvalid] Name
nameOkay name = case name /= "" of
  True -> Right name
  False -> Left [NameEmpty]

-- mkPerson :: Name -> Age -> ValidatePerson Person
-- mkPerson name age =
--   mkPerson' (nameOkay name) (ageOkay age)

mkPerson' :: ValidatePerson Name -> ValidatePerson Age -> ValidatePerson Person
mkPerson' (Right nameOk) (Right ageOk) = Right (Person nameOk ageOk)
mkPerson' (Left badName) (Left badAge) = Left (badName ++ badAge)
mkPerson' (Left badName) _ = Left badName
mkPerson' _ (Left badAge) = Left badAge

--Later in the book we can replace all that with this function
-- mkPerson :: Name -> Age -> Validation [PersonInvalid] Person
-- mkPerson name age = liftA2 Person (nameOkay name) (ageOkay age)
--
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

id :: a -> a
id a = a

-- example GHCi session above the functions
--
-- >>> notThe "the"
-- Nothing
-- >>> notThe "woot"
-- Just "woot"
notThe :: String -> Maybe String
notThe x = if x == "the" then Nothing else Just x

-- >>> replaceThe "the cow loves us"
-- "a cow loves us"
replaceThe :: String -> String
replaceThe = concatMap(\item -> item ++ " " ) . map (\item -> if item == "the" then "a" else item) . words
  where words [] = [[]]
        words (x:xs) = if x == ' ' then [] : (words xs) else ([x] ++ head (words xs)) : tail (words xs)


-- >>> isJust (Just 1)
-- True
-- >>> isJust Nothing
-- False
isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False

-- >>> isNothing (Just 1)
-- False
-- >>> isNothing Nothing
-- True
isNothing :: Maybe a -> Bool
isNothing = not . isJust


-- >>> maybe 0 (+1) Nothing
-- 0
-- >>> maybe 0 (+1) (Just 1)
-- 2
maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just a) = f a
maybe b _ _ = b

-- >>> fromMaybe 0 Nothing
-- 0
-- >>> fromMaybe 0 (Just 1)
-- 1
fromMaybe :: a -> Maybe a -> a -- Try writing it in terms of the maybe catamorphism
fromMaybe a b = Main.maybe a Main.id b

-- >>> listToMaybe [1,2,3]
-- Just 1
-- >>> listToMaybe []
-- Nothing
listToMaybe :: [a] -> Maybe a
listToMaybe (x:xs) = Just x
listToMaybe [] = Nothing

-- >>> maybeToList (Just 1)
-- [1]
-- >>> maybeToList Nothing
-- []
maybeToList :: Maybe a -> [a]
maybeToList (Just x) = [x]
maybeToList Nothing = []

-- >>> catMaybes [Just 1, Nothing, Just 2]
-- [1, 2]
-- >>> catMaybes [Nothing, Nothing, Nothing]
-- []
catMaybes :: [Maybe a] -> [a]
catMaybes = foldr(\item acc -> (maybeToList item) ++ acc) []

-- >>> flipMaybe [Just 1, Just 2, Just 3]
-- Just [1, 2, 3]
-- >>> flipMaybe [Just 1, Nothing, Just 3]
-- Nothing
flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe [] = Just []
flipMaybe ((Just x):xs) = let nextOne = flipMaybe xs in
                            case nextOne of
                              Just xs -> Just (x:xs)
                              Nothing -> Nothing
flipMaybe (Nothing:_ ) = Nothing

lefts' :: [Either a b] -> [a]
lefts' = foldr(\item acc -> (getItem item) ++ acc) []
  where getItem (Left a) = [a]
        getItem (Right _) = []

rights' :: [Either a b] -> [b]
rights' = foldr(\item acc -> (getItem item) ++ acc) []
  where getItem (Left _) = []
        getItem (Right a) = [a]


partitionEithers' :: [Either a b] -> ([a], [b])
partitionEithers' xs = ((lefts' xs), (rights' xs))

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' _ (Left _) = Nothing
eitherMaybe' f (Right x) = Just (f x)

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left x) = f x
either' _ f (Right x) = f x

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f x = either' (\x -> Nothing) (\x -> Just (f x)) x

myIterate :: (a -> a) -> a -> [a]
myIterate f x = x : (myIterate f (f x))

myUnfoldr :: (b -> Maybe (a,b)) -> b -> [a]
myUnfoldr f x =  case (f x) of
                  Just (a,b) -> a:(myUnfoldr f b)
                  Nothing -> []


betterIterate :: (a -> a) -> a -> [a]
betterIterate f x = myUnfoldr (\x -> Just (x, (f x))) x

data BinaryTree a =
      Leaf
    | Node (BinaryTree a) a (BinaryTree a)
    deriving (Eq, Ord, Show)

unfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
unfold f x = case (f x) of
             Just (leftBranch, value, rightBranch) -> Node (unfold f leftBranch) value (unfold f rightBranch)
             Nothing -> Leaf

treeBuild :: Integer -> BinaryTree Integer
treeBuild limit = unfold (\x -> if x < limit then Just(x+1,x,x+1) else Nothing) 0
