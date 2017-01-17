module FollowingAlong where

possiblyKaboom = \f -> f fst snd (0, undefined)

true :: a -> a -> a
true = \a -> (\b -> a)

false :: a -> a -> a
false = \a -> (\b -> b)

-- possiblyKaboom true
-- 0

-- Break Down:
-- (\f -> f fst snd (0, undefined)) (\a -> (\b -> a))
-- (\a -> (\b -> a)) fst snd (0, undefined)
-- (\b -> fst) snd (0, undefined)
-- fst (0, undefined)
-- 0

possiblyKaboom' b =
    case b of
      True -> fst tup
      False -> snd tup
    where tup = (0, undefined)


