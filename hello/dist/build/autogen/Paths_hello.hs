module Paths_hello (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/mac10688/Programming/LearnHaskell/hello/.cabal-sandbox/bin"
libdir     = "/home/mac10688/Programming/LearnHaskell/hello/.cabal-sandbox/lib/x86_64-linux-ghc-7.8.4/hello-0.1.0.0"
datadir    = "/home/mac10688/Programming/LearnHaskell/hello/.cabal-sandbox/share/x86_64-linux-ghc-7.8.4/hello-0.1.0.0"
libexecdir = "/home/mac10688/Programming/LearnHaskell/hello/.cabal-sandbox/libexec"
sysconfdir = "/home/mac10688/Programming/LearnHaskell/hello/.cabal-sandbox/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "hello_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "hello_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "hello_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hello_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hello_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
