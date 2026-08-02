{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
#if __GLASGOW_HASKELL__ >= 810
{-# OPTIONS_GHC -Wno-prepositive-qualified-module #-}
#endif
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_lumenscript_studio (
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
bindir     = "D:\\Arpita\\VS\\GameScript-Studio\\lumenscript-studio\\.stack-work\\install\\3bbdd772\\bin"
libdir     = "D:\\Arpita\\VS\\GameScript-Studio\\lumenscript-studio\\.stack-work\\install\\3bbdd772\\lib\\x86_64-windows-ghc-9.6.6\\lumenscript-studio-0.1.0.0-9ZAsUK9JWrqA2uwmmH3D5g"
dynlibdir  = "D:\\Arpita\\VS\\GameScript-Studio\\lumenscript-studio\\.stack-work\\install\\3bbdd772\\lib\\x86_64-windows-ghc-9.6.6"
datadir    = "D:\\Arpita\\VS\\GameScript-Studio\\lumenscript-studio\\.stack-work\\install\\3bbdd772\\share\\x86_64-windows-ghc-9.6.6\\lumenscript-studio-0.1.0.0"
libexecdir = "D:\\Arpita\\VS\\GameScript-Studio\\lumenscript-studio\\.stack-work\\install\\3bbdd772\\libexec\\x86_64-windows-ghc-9.6.6\\lumenscript-studio-0.1.0.0"
sysconfdir = "D:\\Arpita\\VS\\GameScript-Studio\\lumenscript-studio\\.stack-work\\install\\3bbdd772\\etc"

getBinDir     = catchIO (getEnv "lumenscript_studio_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "lumenscript_studio_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "lumenscript_studio_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "lumenscript_studio_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "lumenscript_studio_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "lumenscript_studio_sysconfdir") (\_ -> return sysconfdir)



joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (List.last dir) = dir ++ fname
  | otherwise                       = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
