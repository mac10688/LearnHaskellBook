module WriteTheCode where

import Control.Monad.Trans.Reader
import Control.Monad.Trans.State
import Data.Functor.Identity

rDec :: Num a => Reader a a
rDec = ReaderT $ \r -> Identity (r - 1)

rDec' :: Num a => Reader a a
rDec' = ReaderT ( Identity . flip (-) 1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ \a -> Identity (show a)

rShow' :: Show a => ReaderT a Identity String
rShow' = ReaderT (Identity . show)

rPrintAndInc :: (Num a, Show a) => ReaderT a IO a
rPrintAndInc = ReaderT $ \a -> do
    putStr "Hi: "
    putStrLn $ show a
    return (a + 1)

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s -> do
    putStr "Hi: "
    putStrLn $ show s
    return ((show s), s+1)
    
