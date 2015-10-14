fibs = 1 : scanl (+) 1 fibs
fibs' = take 20 (1: scanl (+) 1 fibs')
fibs'' = takeWhile (<100) $ (1: scanl (+) 1 fibs'')

fact = scanl (*) 1 [2..]
