k (x, y) = x
k1 = k((4-1), 10)
k2 = k ("three", (1 + 2))
k3 = k (3, True)

--a. k is (a, b) -> a
--b. String
--c. k3

-- Remember: Tuples have the same syntax for their
-- type constructors and their data constructors

f :: (a, b, c) -> (d, e, f) -> ((a,d), (c, f))
f (a, _, c) (d, _, f) = ((a,d), (c,f))

funcZ x = case x+1 == 1 of
  True -> "AWESOME"
  False -> "wut"

pal xs =
  case xs == reverse xs of
    True -> "yes"
    False -> "no"

pal' xs =
  case y of
    True -> "yes"
    False -> "no"
  where y = xs == reverse xs
