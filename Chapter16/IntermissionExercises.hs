a = (+1) $ read "1"

b = (fmap . fmap) (++ "lol") $ (Just ["Hi, ", "Hello"])

c = (*2) . (\x -> x -2)

d = ((return '1' ++) . show) . (\x -> [x, 1..3])

e :: IO Integer
e = let ioi = readIO "1" :: IO Integer
        changed = fmap (\x -> read ("123" ++ (show x)) :: Integer) ioi
    in fmap (*3) changed
