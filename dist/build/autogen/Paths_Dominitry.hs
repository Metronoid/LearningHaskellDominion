{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_Dominitry (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where

import qualified Control.Exception as Exception
import Data.Version (Version(..))
import System.Environment (getEnv)
import Prelude

#if defined(VERSION_base)

#if MIN_VERSION_base(4,0,0)
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#else
catchIO :: IO a -> (Exception.Exception -> IO a) -> IO a
#endif

#else
catchIO :: IO a -> (Exception.IOException -> IO a) -> IO a
#endif
catchIO = Exception.catch

version :: Version
version = Version [0,1,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "/home/wander/.cabal/bin"
libdir     = "/home/wander/.cabal/lib/x86_64-linux-ghc-8.0.2/Dominitry-0.1.0.0"
dynlibdir  = "/home/wander/.cabal/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/wander/.cabal/share/x86_64-linux-ghc-8.0.2/Dominitry-0.1.0.0"
libexecdir = "/home/wander/.cabal/libexec"
sysconfdir = "/home/wander/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "Dominitry_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "Dominitry_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "Dominitry_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "Dominitry_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "Dominitry_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "Dominitry_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
