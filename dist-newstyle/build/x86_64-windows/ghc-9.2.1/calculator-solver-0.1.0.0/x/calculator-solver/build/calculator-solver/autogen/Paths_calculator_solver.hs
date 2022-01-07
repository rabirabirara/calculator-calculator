{-# LANGUAGE CPP #-}
{-# LANGUAGE NoRebindableSyntax #-}
{-# OPTIONS_GHC -fno-warn-missing-import-lists #-}
{-# OPTIONS_GHC -w #-}
module Paths_calculator_solver (
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

getDataFileName :: FilePath -> IO FilePath
getDataFileName name = do
  dir <- getDataDir
  return (dir `joinFileName` name)

getBinDir, getLibDir, getDynLibDir, getDataDir, getLibexecDir, getSysconfDir :: IO FilePath



bindir, libdir, dynlibdir, datadir, libexecdir, sysconfdir :: FilePath
bindir     = "D:\\cabal-packages\\bin"
libdir     = "D:\\cabal-packages\\x86_64-windows-ghc-9.2.1\\calculator-solver-0.1.0.0-inplace-calculator-solver"
dynlibdir  = "D:\\cabal-packages\\x86_64-windows-ghc-9.2.1"
datadir    = "D:\\cabal-packages\\x86_64-windows-ghc-9.2.1\\calculator-solver-0.1.0.0"
libexecdir = "D:\\cabal-packages\\calculator-solver-0.1.0.0-inplace-calculator-solver\\x86_64-windows-ghc-9.2.1\\calculator-solver-0.1.0.0"
sysconfdir = "D:\\cabal-packages\\etc"

getBinDir     = catchIO (getEnv "calculator_solver_bindir")     (\_ -> return bindir)
getLibDir     = catchIO (getEnv "calculator_solver_libdir")     (\_ -> return libdir)
getDynLibDir  = catchIO (getEnv "calculator_solver_dynlibdir")  (\_ -> return dynlibdir)
getDataDir    = catchIO (getEnv "calculator_solver_datadir")    (\_ -> return datadir)
getLibexecDir = catchIO (getEnv "calculator_solver_libexecdir") (\_ -> return libexecdir)
getSysconfDir = catchIO (getEnv "calculator_solver_sysconfdir") (\_ -> return sysconfdir)




joinFileName :: String -> String -> FilePath
joinFileName ""  fname = fname
joinFileName "." fname = fname
joinFileName dir ""    = dir
joinFileName dir fname
  | isPathSeparator (last dir) = dir ++ fname
  | otherwise                  = dir ++ pathSeparator : fname

pathSeparator :: Char
pathSeparator = '\\'

isPathSeparator :: Char -> Bool
isPathSeparator c = c == '/' || c == '\\'
