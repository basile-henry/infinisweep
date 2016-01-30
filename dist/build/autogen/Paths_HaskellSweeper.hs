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

bindir     = "/media/DATA/Dev/HaskellSweeper/.cabal-sandbox/bin"
libdir     = "/media/DATA/Dev/HaskellSweeper/.cabal-sandbox/lib/x86_64-linux-ghc-7.10.3/HaskellSweeper-0.1.0.0-8yrWHMeZwRZ6ev8h2S7V5S"
datadir    = "/media/DATA/Dev/HaskellSweeper/.cabal-sandbox/share/x86_64-linux-ghc-7.10.3/HaskellSweeper-0.1.0.0"
libexecdir = "/media/DATA/Dev/HaskellSweeper/.cabal-sandbox/libexec"
sysconfdir = "/media/DATA/Dev/HaskellSweeper/.cabal-sandbox/etc"

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
