module Paths_AdHocParser (
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
version = Version [1,0] []
bindir, libdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "D:\\Development\\workspace\\Stackbuilders\\AdHocParser\\.stack-work\\install\\89b54721\\bin"
libdir     = "D:\\Development\\workspace\\Stackbuilders\\AdHocParser\\.stack-work\\install\\89b54721\\lib\\x86_64-windows-ghc-7.10.3\\AdHocParser-1.0-Hg5TUHUolndDgcAzWPmM2L"
datadir    = "D:\\Development\\workspace\\Stackbuilders\\AdHocParser\\.stack-work\\install\\89b54721\\share\\x86_64-windows-ghc-7.10.3\\AdHocParser-1.0"
libexecdir = "D:\\Development\\workspace\\Stackbuilders\\AdHocParser\\.stack-work\\install\\89b54721\\libexec"
sysconfdir = "D:\\Development\\workspace\\Stackbuilders\\AdHocParser\\.stack-work\\install\\89b54721\\etc"

getBinDir, getLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "AdHocParser_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "AdHocParser_libdir") (\_ -> return libdir)
getDataDir = catchIO (getEnv "AdHocParser_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "AdHocParser_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "AdHocParser_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
