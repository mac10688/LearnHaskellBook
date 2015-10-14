f :: Bool -> Int
f True = error "blah"
f False = 0

f' :: Bool -> Int
f' False = 0
