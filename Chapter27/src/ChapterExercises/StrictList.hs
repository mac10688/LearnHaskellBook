{- LANGUAGE Strict #-}

module StrictList where

data List a = Nil | Cons a (List a) deriving (Show)

take' n _ | n <= 0 = Nil
take' _ Nil = Nil
take' n (Cons x xs) = (Cons x (take' (n-1) xs))

map' _ Nil = Nil
map' f (Cons x xs) = (Cons (f x) (map' f xs))

repeat' x = xs where xs = (Cons x xs)

main = do
    print $ take' 10 $ map' (+1) (repeat' 1)


{- 
    1. let x = 1
    :sprint x
    x = _

    2. let x = ['1']
    :sprint x
    x = "1"

    3. let x = [1]
    :sprint x
    x = _

    4. let x = 1 :: Int
    :sprint x
    x = 1

    5. let f = \x -> x
    let x = f 1
    :sprint x
    x = _

    6. let f :: Int -> Int; f = \x -> x
    let x = f 1
    :sprint x
    x = _


    Will printing this epxression result in bottom?

    1. snd (undefined, 1)
    no

    2. let x = undefined
       let y = x `seq` 1 in snd (x, y)
    yes

    3. length $ [1..5] ++ undefined
    yes

    4. length $ [1..5] ++ [undefined]
    no

    5. const 1 undefined
    no

    6. const 1 (undefined `seq` 1)
    no

    7. const undefined 1
    yes

    
    -}
    
-- Using only bang patterns or seq, make the code bottom out when executed.
x = undefined
y = "blah"
main' = do
    print (snd (x , y))

main'' = do
    print (snd (x , x `seq` y))

