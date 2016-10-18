module Morra where

import Data.Char
import Control.Monad.Trans.State
import System.Random
import Control.Monad.Trans.Class

data GameScore = GameScore {
                               player :: Int
                             , computer :: Int
                           } deriving (Eq, Show)

type GameState = StateT GameScore IO


startGame :: GameState ()
startGame = do
    lift printPrompt
    response <- lift getLine 
    case response of
       "q" -> do 
            lift $ putStrLn "User pressed q" 
            return ()
       x | all isDigit x -> do
           let number = read x :: Int
           case number of
              x | x < 0 || x > 10 -> startGame
              otherwise -> do 
                  lift $ putStrLn ("P: " ++ (show number))
                  cNum <- lift $ (randomRIO (0, 10) :: IO Int)
                  lift $ putStrLn ("C: " ++ (show cNum))
                  let sum = cNum + number
                  if even sum then
                     do
                         lift $ putStrLn "C wins"
                         state <- get 
                         put GameScore { player = player state, computer = (computer state) + 1}
                  else
                     do
                         lift $ putStrLn "P wins"
                         state <- get
                         put GameScore { player = (player state) + 1, computer = computer state}
           startGame
       otherwise -> do 
           startGame
          


printPrompt :: IO ()
printPrompt = putStrLn "Please enter a number between 0-10 or 'q' to quit."

initialState :: GameScore
initialState = GameScore { player = 0, computer = 0}

main :: IO GameScore
main = execStateT startGame initialState
