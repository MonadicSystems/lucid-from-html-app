{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
module Paths_lucid_from_html_app (
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

bindir     = "/root/.cabal/bin"
libdir     = "/root/.cabal/lib/x86_64-linux-ghc-8.6.5/lucid-from-html-app-0.1.0.0-inplace"
dynlibdir  = "/root/.cabal/lib/x86_64-linux-ghc-8.6.5"
datadir    = "/root/.cabal/share/x86_64-linux-ghc-8.6.5/lucid-from-html-app-0.1.0.0"
libexecdir = "/root/.cabal/libexec/x86_64-linux-ghc-8.6.5/lucid-from-html-app-0.1.0.0"
sysconfdir = "/root/.cabal/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "lucid_from_html_app_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "lucid_from_html_app_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "lucid_from_html_app_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "lucid_from_html_app_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lucid_from_html_app_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lucid_from_html_app_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
