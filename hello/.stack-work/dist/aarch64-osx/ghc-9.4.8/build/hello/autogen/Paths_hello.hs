{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_hello (
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
bindir     = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/hello/.stack-work/install/aarch64-osx/a71ae160883c9e15c8fd4b22fc47c21c5b0b22221c31bb4d66b3bd59b21921b8/9.4.8/bin"
libdir     = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/hello/.stack-work/install/aarch64-osx/a71ae160883c9e15c8fd4b22fc47c21c5b0b22221c31bb4d66b3bd59b21921b8/9.4.8/lib/aarch64-osx-ghc-9.4.8/hello-0.1.0.0-2wwGsHuNnpt5Z4ZcIT94qa-hello"
dynlibdir  = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/hello/.stack-work/install/aarch64-osx/a71ae160883c9e15c8fd4b22fc47c21c5b0b22221c31bb4d66b3bd59b21921b8/9.4.8/lib/aarch64-osx-ghc-9.4.8"
datadir    = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/hello/.stack-work/install/aarch64-osx/a71ae160883c9e15c8fd4b22fc47c21c5b0b22221c31bb4d66b3bd59b21921b8/9.4.8/share/aarch64-osx-ghc-9.4.8/hello-0.1.0.0"
libexecdir = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/hello/.stack-work/install/aarch64-osx/a71ae160883c9e15c8fd4b22fc47c21c5b0b22221c31bb4d66b3bd59b21921b8/9.4.8/libexec/aarch64-osx-ghc-9.4.8/hello-0.1.0.0"
sysconfdir = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/hello/.stack-work/install/aarch64-osx/a71ae160883c9e15c8fd4b22fc47c21c5b0b22221c31bb4d66b3bd59b21921b8/9.4.8/etc"

getBinDir     = catchIO (getEnv "hello_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "hello_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "hello_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "hello_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "hello_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "hello_sysconfdir") (\_ -> return sysconfdir)




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
