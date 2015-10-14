module Paths_call_em_up (
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

bindir     = "/home/mac10688/.cabal/bin"
libdir     = "/home/mac10688/.cabal/lib/x86_64-linux-ghc-7.8.4/call-em-up-0.1.0.0"
datadir    = "/home/mac10688/.cabal/share/x86_64-linux-ghc-7.8.4/call-em-up-0.1.0.0"
libexecdir = "/home/mac10688/.cabal/libexec"
sysconfdir = "/home/mac10688/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "call_em_up_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "call_em_up_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "call_em_up_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "call_em_up_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "call_em_up_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
