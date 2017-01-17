{-


1.
const 1 undefined
const x y = x
const 1 undefined = 1

2.
const undefined 1
const x y = x
const undefined 1 = undefined

3.
flip f v1 v2 = f v2 v1
flip const undefined 1

c = const undefined 1 = const 1 undefined = 1

4.
flip const 1 undefined
flip f v1 v2 = v2 v1

c= const 1 undefined = const undefined 1 = undefined

5.
const undefined undefined
const x y = x
const undefined undefined = undefined

6.
    foldr k z xs = go xs
        where
            go [] = z
            go (y:ys) = y `k` go ys

    c = foldr const 'z' ['a' .. 'e']

    c = const 'z' "abcde" = go "abcde"
        where
            go [] = 'z'
            go ('a' : "bcde") = 'a' `const` go "bcde"


    -- so the first step of evaluating
    -- of the fold here is:
    
    const 'a' (go "bcde")

    const x y = x

    const 'a' _ = 'a'

    foldr const 'z' ['a', undefined]
    'a'

7.
    foldr (flip const) 'z' ['a' .. 'e']

    foldr k z xs = go xs
        where 
            go [] = z
            go (y:ys) = y `k` go ys

    flip f p1 p2 = f p2 p1

    const a b = a

    flip const a b = b

    c = foldr (flip const) 'z' ['a' .. 'e']
    c = (flip const) 'z' ['a' .. 'e'] = go "abcde"
        where
            go [] = 'z'
            go ('a':"bcde") = (flip const) 'a' (go "bcde")

    (flip const) 'a' (go "bcde) = go "bcde"

    foldr (flip const) 'z' ['a' .. 'e'] = z
    
