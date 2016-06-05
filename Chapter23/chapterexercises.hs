{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}
newtype State s a = State { runState :: s -> (a, s) }

instance Functor (State s) where
    fmap :: (a -> b) -> State s a -> State s b
    fmap f (State g) = State (\r -> let (a,s) = (g r) in ((f a), s) )

instance Applicative (State s) where
    pure :: a -> State s a
    pure a = State (\r -> (a, r))

    (<*>) :: State s (a -> b) -> State s a -> State s b
    (State f) <*> (State g) = State (\r -> 
                                    let (a,s) = (g r) 
                                        (f', s') = (f s)
                                    in ((f' a), s'))

instance Monad (State s) where
    return = pure

    (>>=) :: State s a -> (a -> State s b) -> State s b
    (State f) >>= g = State (\r -> let (a, s) = (f r) in (runState (g a)) s)

get :: State s s
get = State (\r -> (r,r))

--runState get "curryIsAmaze"
--("curryIsAmaze", "curryIsAmaze")

put :: s -> State s ()
put s = State (\r -> ((), s))

--runState (put "blah") "woot"
--((), "blah)

exec :: State s a -> s -> s
exec (State sa) s = snd (sa s)

--exec (put "wilma") "daphne"
--"wilma"
--exec get "scooby papu"
--"scooby papu"

eval :: State s a -> s -> a
eval (State sa) s = fst (sa s)

--eval get "bunnicula"
--bunnicula
--eval get "stake a bunny"
--"stake a bunny"

modify :: (s -> s) -> State s ()
modify f = State (\r -> ((), (f r)))

-- runState (modify (+1)) 0
-- ((),1)
-- runState (modify (+1) >> modify (+1)) 0
-- ((), 2)
