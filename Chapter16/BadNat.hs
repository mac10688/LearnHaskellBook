module BadNate where

type Nat f g a = f a -> g a

-- This'll work
maybeToList :: Nat Maybe [] a
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- But this will too if we tell it
-- 'a' is Num a => a

degenerateMtl :: Num a => Nat Maybe [] a
degenerateMtl Nothing = []
degenerateMtl (Just a) = [a+1]
