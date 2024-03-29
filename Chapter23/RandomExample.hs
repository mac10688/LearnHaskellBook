{-# LANGUAGE InstanceSigs #-}

module RandomExample where

import Control.Applicative (liftA3)
import Control.Monad (replicateM)
import Control.Monad.Trans.State
import System.Random

-- six sided-die
data Die =
    DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
    case n of
     1 -> DieOne
     2 -> DieTwo
     3 -> DieThree
     4 -> DieFour
     5 -> DieFive
     6 -> DieSix
     -- use this tactic_extremely_sparingly
     x -> error $ "intToDie got non 1-6 integer" ++ show x

rollDieThreeTimes :: (Die, Die, Die)
rollDieThreeTimes = do
    let s = mkStdGen 0
        (d1, s1) = randomR (1,6) s
        (d2, s2) = randomR (1,6) s1
        (d3, _) = randomR (1,6) s2
    (intToDie d1, intToDie d2, intToDie d3)

rollDie :: State StdGen Die
rollDie = state $ do
    (n, s) <- randomR (1, 6)
    return (intToDie n, s)

rollDie' :: State StdGen Die
rollDie' = intToDie <$> state (randomR (1,6))

rollDieThreeTimes' :: State StdGen (Die, Die, Die)
rollDieThreeTimes' = liftA3 (,,) rollDie' rollDie' rollDie'

infiniteDie :: State StdGen [Die]
infiniteDie = repeat <$> rollDie

nDie :: Int -> State StdGen [Die]
nDie n = replicateM n rollDie

rollsToGetTwenty :: StdGen -> Int
rollsToGetTwenty g = go 0 0 g
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
           | sum >= 20 = count
           | otherwise = 
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

-- Practice Exercises
rollsToGetN :: Int -> StdGen -> Int
rollsToGetN n g = go 0 0 g
    where go :: Int -> Int -> StdGen -> Int
          go sum count gen
            | sum >= n = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

rollsCountLogged :: Int -> StdGen -> (Int, [Die])
rollsCountLogged n g = go 0 0 [] g
    where go :: Int -> Int -> [Die] -> StdGen -> (Int, [Die])
          go sum count history gen
            | sum >= n = (count, history)
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen in
                go (sum + die) (count + 1) ((intToDie die):history) nextGen

newtype Moi s a = Moi { runMoi :: s -> (a, s) }

instance Functor (Moi s) where
    fmap :: (a -> b) -> Moi s a -> Moi s b
    fmap f (Moi g) = Moi (\r -> let (a,s) = (g r) in ((f a), s) )

instance Applicative (Moi s) where
    pure :: a -> Moi s a
    pure a = Moi (\r -> (a, r))

    (<*>) :: Moi s (a -> b) -> Moi s a -> Moi s b
    (Moi f) <*> (Moi g) = Moi (\r -> 
                                    let (a,s) = (g r) 
                                        (f', s') = (f s)
                                    in ((f' a), s'))

instance Monad (Moi s) where
    return = pure

    (>>=) :: Moi s a -> (a -> Moi s b) -> Moi s b
    (Moi f) >>= g = Moi (\r -> let (a, s) = (f r) in (runMoi (g a)) s)
