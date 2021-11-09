{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_mp5_scheme (
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
version = Version [0,2,0,0] []
bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath

bindir     = "C:\\Users\\seanm\\Documents\\fall2021Semester\\CS440F2021\\MPs\\mp5-scheme\\.stack-work\\install\\34edf8d2\\bin"
libdir     = "C:\\Users\\seanm\\Documents\\fall2021Semester\\CS440F2021\\MPs\\mp5-scheme\\.stack-work\\install\\34edf8d2\\lib\\x86_64-windows-ghc-8.10.4\\mp5-scheme-0.2.0.0-5K4wqBsnvfjHExvCNdZg56"
dynlibdir  = "C:\\Users\\seanm\\Documents\\fall2021Semester\\CS440F2021\\MPs\\mp5-scheme\\.stack-work\\install\\34edf8d2\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\seanm\\Documents\\fall2021Semester\\CS440F2021\\MPs\\mp5-scheme\\.stack-work\\install\\34edf8d2\\share\\x86_64-windows-ghc-8.10.4\\mp5-scheme-0.2.0.0"
libexecdir = "C:\\Users\\seanm\\Documents\\fall2021Semester\\CS440F2021\\MPs\\mp5-scheme\\.stack-work\\install\\34edf8d2\\libexec\\x86_64-windows-ghc-8.10.4\\mp5-scheme-0.2.0.0"
sysconfdir = "C:\\Users\\seanm\\Documents\\fall2021Semester\\CS440F2021\\MPs\\mp5-scheme\\.stack-work\\install\\34edf8d2\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "mp5_scheme_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "mp5_scheme_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "mp5_scheme_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "mp5_scheme_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "mp5_scheme_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "mp5_scheme_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
