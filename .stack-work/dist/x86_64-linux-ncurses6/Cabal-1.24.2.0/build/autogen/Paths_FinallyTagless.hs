{-# LANGUAGE CPP #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -fno-warn-implicit-prelude #-}
module Paths_FinallyTagless (
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

bindir     = "/home/doyougnu/School_Files/Spring_2017/Modularity/Projects/FinallyTagless/.stack-work/install/x86_64-linux-ncurses6/lts-8.15/8.0.2/bin"
libdir     = "/home/doyougnu/School_Files/Spring_2017/Modularity/Projects/FinallyTagless/.stack-work/install/x86_64-linux-ncurses6/lts-8.15/8.0.2/lib/x86_64-linux-ghc-8.0.2/FinallyTagless-0.1.0.0-IYylgWihXo27eRWIlROeOz"
dynlibdir  = "/home/doyougnu/School_Files/Spring_2017/Modularity/Projects/FinallyTagless/.stack-work/install/x86_64-linux-ncurses6/lts-8.15/8.0.2/lib/x86_64-linux-ghc-8.0.2"
datadir    = "/home/doyougnu/School_Files/Spring_2017/Modularity/Projects/FinallyTagless/.stack-work/install/x86_64-linux-ncurses6/lts-8.15/8.0.2/share/x86_64-linux-ghc-8.0.2/FinallyTagless-0.1.0.0"
libexecdir = "/home/doyougnu/School_Files/Spring_2017/Modularity/Projects/FinallyTagless/.stack-work/install/x86_64-linux-ncurses6/lts-8.15/8.0.2/libexec"
sysconfdir = "/home/doyougnu/School_Files/Spring_2017/Modularity/Projects/FinallyTagless/.stack-work/install/x86_64-linux-ncurses6/lts-8.15/8.0.2/etc"

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath
getBinDir = catchIO (getEnv "FinallyTagless_bindir") (\_ -> return bindir)
getLibDir = catchIO (getEnv "FinallyTagless_libdir") (\_ -> return libdir)
getDynLibDir = catchIO (getEnv "FinallyTagless_dynlibdir") (\_ -> return dynlibdir)
getDataDir = catchIO (getEnv "FinallyTagless_datadir") (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "FinallyTagless_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "FinallyTagless_sysconfdir") (\_ -> return sysconfdir)

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir ++ "/" ++ name)
