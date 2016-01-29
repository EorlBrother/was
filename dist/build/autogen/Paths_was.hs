module Paths_was (
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
version = Version [0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,1] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/david/.cabal/bin"
libdir     = "/home/david/.cabal/lib/x86_64-linux-ghc-7.10.3/was-0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.1-BB3WaYyRqOdABZ5QLHTQjv"
datadir    = "/home/david/.cabal/share/x86_64-linux-ghc-7.10.3/was-0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.0.1"
libexecdir = "/home/david/.cabal/libexec"
sysconfdir = "/home/david/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "was_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "was_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "was_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "was_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "was_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
