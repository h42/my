module Paths_my (
    version,
    getBinDir, getLibDir, getDataDir, getLibexecDir,
    getDataFileName
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
catchIO = Exception.catch


version :: Version
version = Version {versionBranch = [0,1], versionTags = []}
bindir, libdir, datadir, libexecdir :: FilePath

bindir     = "/home/jerry/.cabal/bin"
libdir     = "/home/jerry/.cabal/lib/x86_64-linux-ghc-7.6.3/my-0.1"
datadir    = "/home/jerry/.cabal/share/x86_64-linux-ghc-7.6.3/my-0.1"
libexecdir = "/home/jerry/.cabal/libexec"

getBinDir, getLibDir, getDataDir, getLibexecDir :: IO FilePath
getBinDir = catchIO (getEnv "my_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "my_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "my_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "my_libexecdir") (\_ -> return libexecdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
