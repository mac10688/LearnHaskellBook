{-# LANGUAGE OverloadedStrings #-}

module HitCounter where

import Control.Monad.Trans.Class
import Control.Monad.Trans.Reader
import Data.IORef
import qualified Data.Map as M
import Data.Maybe (fromMaybe)
import Data.Text.Lazy (Text)
import qualified Data.Text.Lazy as TL
import System.Environment (getArgs)
import Web.Scotty.Trans

data Config =
    Config {
           -- that's one, one click!
           -- two...two clicks!
           -- Three BEAUTIFUL clicks! ah ah ahhh
           counts :: IORef (M.Map Text Integer)
         , prefix :: Text
        }

-- Stuff inside ScottyT is, except for things that escape
-- via IO, effectively read-only so we can't use StateT.
-- It would overcomplicate things to attempt to do so and
-- you should be using a proper database for production
-- applications.

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

bumpBoomp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
bumpBoomp k m = case M.lookup k m of
                  Just value -> (M.insert k (value + 1) m, value)
                  Nothing -> (M.insert k 1 m, 0)



app :: Scotty ()
app = 
    get "/:key" $ do
        unprefixed <- param "key"
        config <- lift ask
        let prefix' = prefix config
            key' = mappend prefix' unprefixed
            ioRefMap = counts config
        map <- (lift . lift) $ readIORef ioRefMap
        let (newMap, newInteger) = bumpBoomp key' map
        (lift . lift) $ writeIORef ioRefMap newMap
        html $ mconcat [ "<h1>Success! Count was: "
                       , TL.pack $ show newInteger
                       , "</h1>"
                       ]


main :: IO ()
main = do
    prefixArg <- Prelude.head <$> getArgs
    counter <- newIORef M.empty
    let config = Config counter (TL.pack prefixArg)
        runR ma = (runReaderT ma) config
    scottyT 3000 runR app
