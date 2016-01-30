module Paths_HaskellSweeper (
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

bindir     = "/home/basile/.cabal/bin"
libdir     = "/home/basile/.cabal/lib/x86_64-linux-ghc-7.10.3/HaskellSweeper-0.1.0.0-D9IAHIxbWle3BA6bySuJ1S"
datadir    = "/home/basile/.cabal/share/x86_64-linux-ghc-7.10.3/HaskellSweeper-0.1.0.0"
libexecdir = "/home/basile/.cabal/libexec"
sysconfdir = "/home/basile/.cabal/etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "HaskellSweeper_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "HaskellSweeper_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "HaskellSweeper_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "HaskellSweeper_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "HaskellSweeper_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
