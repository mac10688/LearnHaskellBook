f1 :: String -> String
f1 x = x ++ "!"

f2 :: [a] -> [a]
f2 x = [x !! 4]

f3 :: [a] -> [a]
f3 x = drop 9 x

thirdLetter :: String -> Char
thirdLetter x = x !! 2

letterIndex :: Int -> Char
letterIndex x = "Curry is awesome!" !! x

rvrs :: String -> String
rvrs x = drop 9 x ++ take 4 ( drop 5 x) ++ take 5 x
