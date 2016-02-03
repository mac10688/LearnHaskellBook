-- | A library to do stuff.
module Lib where

import qualified Data.Map as M

type Morse = String

letterToMorse :: (M.Map Char Morse)
letterToMorse = M.fromList [
                ('a', ".-")
              , ('b', "-...")
              , ('c', "-.-.")
              , ('d', "-..")
              , ('e', ".")
              , ('f', "..-.")
              , ('g', "--.")
              , ('h', "....")
              , ('i', "..")
              , ('j', ".---")
              , ('k', "-.-")
              , ('l', ".-..")
              , ('m', "--")
              , ('n', "-.")
              , ('o', "---")
              , ('p', ".--.")
              , ('q', "--.-")
              , ('r', ".-.")
              , ('s', "...")
              , ('t', "-")
              , ('u', "..-")
              , ('v', "...-")
              , ('w', ".--")
              , ('x', "-..-")
              , ('y', "-.--")
              , ('z', "--..")
              , ('1', ".----")
              , ('2', "..---")
              , ('3', "...--")
              , ('4', "....-")
              , ('5', ".....")
              , ('6', "-....")
              , ('7', "--...")
              , ('8', "---..")
              , ('9', "----.")
              , ('0', "-----")
              ]

morseToLetter :: M.Map Morse Char
morseToLetter = M.foldWithKey (flip M.insert) M.empty letterToMorse

charToMorse :: Char -> Maybe Morse
charToMorse c = M.lookup c letterToMorse

--stringToMorse :: String -> Maybe [Morse]
--stringToMorse s = sequence $ fmap charToMorse s

morseToChar :: Morse -> Maybe Char
morseToChar m = M.lookup m morseToLetter



stringToMorse :: String -> Maybe [Morse]
stringToMorse s = sequence $ fmap charToMorse s

stringToMorse' :: String -> Maybe [Morse]
stringToMorse' = traverse charToMorse

-- This code is from Alex Petrov
-- who was kicking something around on Twitter.
-- Thanks for the great example Alex :)

data Query = Query
data SomeObj = SomeObj
data IoOnlyObj = IoOnlyObj
data Err = Err

-- There's a decoder function that makes some object from a string
decodeFn :: String -> Either Err SomeObj
decodeFn = undefined

-- There's a query, that runs against the DB and returns array of strings
fetchFn :: Query -> IO [String]
fetchFn = undefined

-- there's some additional "context initializer", that also has IO side-effects
makeIoOnlyObj :: [SomeObj] -> IO [(SomeObj, IoOnlyObj)]
makeIoOnlyObj = undefined

--before
pipelineFn :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn query = do
    a <- fetchFn query
    case sequence (map decodeFn a) of
        (Left err) -> return $ Left $ err
        (Right res) -> do
            a <- makeIoOnlyObj res
            return $ Right a

pipelineFn' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn' query = do
    a <- fetchFn query
    traverse makeIoOnlyObj (mapM decodeFn a)

pipelineFn'' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn'' = (traverse makeIoOnlyObj . mapM decodeFn =<<) . fetchFn

pipelineFn''' :: Query -> IO (Either Err [(SomeObj, IoOnlyObj)])
pipelineFn''' = (traverse makeIoOnlyObj . traverse decodeFn =<<) . fetchFn
