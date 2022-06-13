{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_haskell_project (
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

bindir     = "/Users/temiedani/Desktop/QMUL/Functional Programming/Twitter API APP/haskell-project/.stack-work/install/x86_64-osx/bce4154d0f1d86b3919a5115e158a6dba381682a1f691ca6cc549912b56254b1/8.10.7/bin"
libdir     = "/Users/temiedani/Desktop/QMUL/Functional Programming/Twitter API APP/haskell-project/.stack-work/install/x86_64-osx/bce4154d0f1d86b3919a5115e158a6dba381682a1f691ca6cc549912b56254b1/8.10.7/lib/x86_64-osx-ghc-8.10.7/haskell-project-0.1.0.0-Iuf5XyrDbN0ElhrbGZTuFN-haskell-project-exe"
dynlibdir  = "/Users/temiedani/Desktop/QMUL/Functional Programming/Twitter API APP/haskell-project/.stack-work/install/x86_64-osx/bce4154d0f1d86b3919a5115e158a6dba381682a1f691ca6cc549912b56254b1/8.10.7/lib/x86_64-osx-ghc-8.10.7"
datadir    = "/Users/temiedani/Desktop/QMUL/Functional Programming/Twitter API APP/haskell-project/.stack-work/install/x86_64-osx/bce4154d0f1d86b3919a5115e158a6dba381682a1f691ca6cc549912b56254b1/8.10.7/share/x86_64-osx-ghc-8.10.7/haskell-project-0.1.0.0"
libexecdir = "/Users/temiedani/Desktop/QMUL/Functional Programming/Twitter API APP/haskell-project/.stack-work/install/x86_64-osx/bce4154d0f1d86b3919a5115e158a6dba381682a1f691ca6cc549912b56254b1/8.10.7/libexec/x86_64-osx-ghc-8.10.7/haskell-project-0.1.0.0"
sysconfdir = "/Users/temiedani/Desktop/QMUL/Functional Programming/Twitter API APP/haskell-project/.stack-work/install/x86_64-osx/bce4154d0f1d86b3919a5115e158a6dba381682a1f691ca6cc549912b56254b1/8.10.7/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "haskell_project_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "haskell_project_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "haskell_project_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "haskell_project_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "haskell_project_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "haskell_project_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
