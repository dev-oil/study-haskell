{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_todo_app (
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
bindir     = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/haskell-todo/todo-app/.stack-work/install/aarch64-osx/e102602ecde8063adbfc1c2fcac9f3e3506b509f9787e14af3a9fcd86763e128/9.8.4/bin"
libdir     = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/haskell-todo/todo-app/.stack-work/install/aarch64-osx/e102602ecde8063adbfc1c2fcac9f3e3506b509f9787e14af3a9fcd86763e128/9.8.4/lib/aarch64-osx-ghc-9.8.4/todo-app-0.1.0.0-FCpis6Gnwzq550nBZfVmb5-todo-app-exe"
dynlibdir  = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/haskell-todo/todo-app/.stack-work/install/aarch64-osx/e102602ecde8063adbfc1c2fcac9f3e3506b509f9787e14af3a9fcd86763e128/9.8.4/lib/aarch64-osx-ghc-9.8.4"
datadir    = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/haskell-todo/todo-app/.stack-work/install/aarch64-osx/e102602ecde8063adbfc1c2fcac9f3e3506b509f9787e14af3a9fcd86763e128/9.8.4/share/aarch64-osx-ghc-9.8.4/todo-app-0.1.0.0"
libexecdir = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/haskell-todo/todo-app/.stack-work/install/aarch64-osx/e102602ecde8063adbfc1c2fcac9f3e3506b509f9787e14af3a9fcd86763e128/9.8.4/libexec/aarch64-osx-ghc-9.8.4/todo-app-0.1.0.0"
sysconfdir = "/Users/yoohwakim/Desktop/Folder/Projects/devdevoildev/study-haskell/haskell-todo/todo-app/.stack-work/install/aarch64-osx/e102602ecde8063adbfc1c2fcac9f3e3506b509f9787e14af3a9fcd86763e128/9.8.4/etc"

getBinDir     = catchIO (getEnv "todo_app_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "todo_app_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "todo_app_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "todo_app_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "todo_app_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "todo_app_sysconfdir") (\_ -> return sysconfdir)



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
