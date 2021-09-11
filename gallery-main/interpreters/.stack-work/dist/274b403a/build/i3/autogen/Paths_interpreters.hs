{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_interpreters (
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

bindir     = "C:\\Users\\Sean McQuillan\\Documents\\CS440f2021\\gallery-main\\interpreters\\.stack-work\\install\\90da2636\\bin"
libdir     = "C:\\Users\\Sean McQuillan\\Documents\\CS440f2021\\gallery-main\\interpreters\\.stack-work\\install\\90da2636\\lib\\x86_64-windows-ghc-8.10.4\\interpreters-0.1.0.0-HGWZVDhefEaLrE1BVYTT6H-i3"
dynlibdir  = "C:\\Users\\Sean McQuillan\\Documents\\CS440f2021\\gallery-main\\interpreters\\.stack-work\\install\\90da2636\\lib\\x86_64-windows-ghc-8.10.4"
datadir    = "C:\\Users\\Sean McQuillan\\Documents\\CS440f2021\\gallery-main\\interpreters\\.stack-work\\install\\90da2636\\share\\x86_64-windows-ghc-8.10.4\\interpreters-0.1.0.0"
libexecdir = "C:\\Users\\Sean McQuillan\\Documents\\CS440f2021\\gallery-main\\interpreters\\.stack-work\\install\\90da2636\\libexec\\x86_64-windows-ghc-8.10.4\\interpreters-0.1.0.0"
sysconfdir = "C:\\Users\\Sean McQuillan\\Documents\\CS440f2021\\gallery-main\\interpreters\\.stack-work\\install\\90da2636\\etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "interpreters_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "interpreters_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "interpreters_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "interpreters_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "interpreters_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "interpreters_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "\\" ++ name)
