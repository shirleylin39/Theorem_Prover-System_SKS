{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_System_SKS (
    version,
    getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir,
    getDataFileName, getSysconfDir
  ) where


import qualified Control.Exception as Exception
import qualified Data.List as List
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath




bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "/Users/shirley/.cabal/bin"
libdir     = "/Users/shirley/.cabal/lib/aarch64-osx-ghc-9.8.2-inplace/System-SKS-0.1.0.0-inplace-benchmark-test"
dynlibdir  = "/Users/shirley/.cabal/lib/aarch64-osx-ghc-9.8.2-inplace"
datadir    = "/Users/shirley/.cabal/share/aarch64-osx-ghc-9.8.2-inplace/System-SKS-0.1.0.0"
libexecdir = "/Users/shirley/.cabal/libexec/aarch64-osx-ghc-9.8.2-inplace/System-SKS-0.1.0.0"
sysconfdir = "/Users/shirley/.cabal/etc"

getBinDir     = catchIO (getEnv "System_SKS_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "System_SKS_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "System_SKS_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "System_SKS_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "System_SKS_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "System_SKS_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '/'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/'
