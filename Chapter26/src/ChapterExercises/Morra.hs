module Morra where

import Data.Char
import Control.Monad.Trans.State
import System.Random

data GameScore = GameScore {
                               player :: Int
                             , computer :: Int
                           }

type GameState = StateT GameScore IO ()


startGame :: IO ()
startGame = do
    printPrompt
    response <- getLine 
    case response of
      "q" -> do 
           putStrLn "User pressed q" 
           return ()
      x | all isDigit x -> do
          let number = read x :: Int
          case number of
            x | x < 0 || x > 10 -> startGame
            otherwise -> do 
                putStrLn ("P: " ++ (show number))
                cNum <- randomRIO (0, 10) :: IO Int
                putStrLn ("C: " ++ (show cNum))
                let sum = cNum + number
                if even sum then
                     putStrLn "C wins"
                else
                     putStrLn "P wins"
          startGame
      otherwise -> do 
          startGame
          


printPrompt :: IO ()
printPrompt = putStrLn "Please enter a number between 0-10 or 'q' to quit."
