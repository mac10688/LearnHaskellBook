module Morra where

import Data.Char
import Control.Monad.Trans.State
import System.Random
import Control.Monad.Trans.Class
import Text.Read (readMaybe)
import System.Console.ANSI

data OddOrEven = Odd | Even deriving (Eq, Show)

data PlayerType = Human | Computer deriving (Eq, Show)

data Player = MkPlayer {
      playerType :: PlayerType
    , numberOfWins :: Int
    , history :: [Int]
} deriving (Eq, Show)

data GameScore = GameScore {
       player1 :: Player
     , player2 :: Player
} deriving (Eq, Show)

type GameState = StateT GameScore IO

setupGame :: IO (Player, Player)
setupGame = do
    player1 <- configureFirstPlayer
    player2 <- configureSecondPlayer
    return (player1, player2)

createPlayer :: PlayerType -> Player
createPlayer pType = MkPlayer {
    playerType = pType
  , history = []
  , numberOfWins = 0
}

configureFirstPlayer :: IO Player
configureFirstPlayer = return $ createPlayer Human

configureSecondPlayer :: IO Player
configureSecondPlayer = do
      playerType <- getPlayerType
      return $ createPlayer playerType


getPlayerType :: IO PlayerType
getPlayerType = do
    putStrLn "Is second player (H)uman or (C)omputer"
    pType <- getLine
    case pType of
      "H" -> return Human
      "C" -> return Computer
      otherwise -> getPlayerType

playRound :: GameState ()
playRound = do
    gameScore <- get
    let p1 = player1 gameScore
    let p2 = player2 gameScore
    p1Guess <- getPlayerGuess p1
    p2Guess <- getPlayerGuess p2
    lift $ putStrLn $ "P1: " ++ (show p1Guess)
    lift $ putStrLn $ "P2: " ++ (show p2Guess)
    let sum = p1Guess + p2Guess
    if (even sum) then
        let 
            p1' = incrementPlayerScore $ addGuessToPlayerHistory p1Guess p1 
        in
        do
            saveState p1' p2
            lift $ putStrLn "P1 wins"
    else
        let 
            p1' = addGuessToPlayerHistory p1Guess p1
            p2' = incrementPlayerScore p2
        in
        do
            saveState p1' p2'
            lift $ putStrLn "P2 wins"
    lift $ putStrLn "Press any key to continue"
    _ <- lift $ getLine
    playRound
    -- lift $ putStrLn "Would you like to (C)ontinue or (Q)uit?"
    -- response <- lift $ getLine
    -- case response of
    --   "C" -> playRound
    --   "Q" -> return ()
    --   otherwise -> playRound

getPlayerGuess :: Player -> GameState Int
getPlayerGuess p = do
    case playerType p of
      Human -> do
        lift $ setCursorPosition 0 0 >> clearScreen
        lift $ putStrLn "Please enter a number between 1-10."
        response <- lift getLine
        case readMaybe response :: Maybe Int of
          Just x | isGuessValid x -> return x
          Nothing -> getPlayerGuess p
      Computer -> do
          history <- history <$> player1 <$> get
          lift $ intelligentComputerGuess history


intelligentComputerGuess :: [Int] -> IO Int
intelligentComputerGuess (fst:snd:xs) = scan fst snd xs
                            where scan fst snd (x:x':x'':xs) = 
                                   if (fst == x' && snd == x'') then
                                        if even x then
                                          return 1
                                        else
                                          return 2
                                   else
                                      scan fst snd (x':x'':xs)
                                  scan _ _ _ = randomRIO (0,10)
intelligentComputerGuess _ = randomRIO (0,10)

incrementPlayerScore :: Player -> Player
incrementPlayerScore p = MkPlayer {
    playerType = (playerType p)
  , numberOfWins = (numberOfWins p) + 1
  , history = (history p)
}

addGuessToPlayerHistory :: Int -> Player -> Player
addGuessToPlayerHistory g p = MkPlayer {
    playerType = (playerType p)
  , numberOfWins = (numberOfWins p)
  , history = g:(history p)
}

saveState :: Player -> Player -> GameState ()
saveState p1 p2 = put GameScore { player1 = p1, player2 = p2 }

quitGame :: GameState ()
quitGame = lift $ putStrLn "Quitting the game"

isGuessValid :: Int -> Bool
isGuessValid x = x >= 0 && x <= 10 

printPrompt :: IO ()
printPrompt = putStrLn "Please enter a number between 0-10 or (Q)uit"

initializeGame :: IO GameScore
initializeGame = do
    (p1, p2) <- setupGame
    return $ GameScore { player1 = p1, player2 = p2}

main :: IO GameScore
main = do 
    startGame <- initializeGame
    execStateT playRound startGame
