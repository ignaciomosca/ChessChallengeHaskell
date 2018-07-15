module Paths_chess (
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

bindir     = "/home/ignacio/.cabal/bin"
libdir     = "/home/ignacio/.cabal/lib/x86_64-linux-ghc-7.10.3/chess-0.1.0.0-C18txI8xzgYKJJk0p51Htb"
datadir    = "/home/ignacio/.cabal/share/x86_64-linux-ghc-7.10.3/chess-0.1.0.0"
libexecdir = "/home/ignacio/.cabal/libexec"
sysconfdir = "/home/ignacio/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "chess_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "chess_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "chess_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "chess_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "chess_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
