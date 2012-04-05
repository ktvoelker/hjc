
module Main where

-- Mostly taken from http://www.haskell.org/haskellwiki/GHC/As_a_library

import DynFlags
import HscTypes (ModGuts)
import GHC
import GHC.Paths (libdir)
import System.Environment (getArgs)
import System.Exit (exitFailure, exitSuccess)

import Compiler
import Output

usage :: String
usage = "Usage: hjc MAIN_SOURCE OUTPUT"
 
main :: IO ()
main = do
  args <- getArgs
  case args of
    [modName, outputName] ->
      getModules modName
      >>= writeModules outputName . map compileModule
      >> exitSuccess
    _ ->
      putStrLn usage >> exitFailure
 
getModules :: String -> IO [ModGuts]
getModules targetName = 
  defaultErrorHandler defaultLogAction $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = foldl xopt_set dflags
                          [Opt_Cpp, Opt_ImplicitPrelude, Opt_MagicHash]
      setSessionDynFlags dflags'
      target <- guessTarget targetName Nothing
      setTargets [target]
      load LoadAllTargets
      ms <- getModuleGraph
      ps <- mapM parseModule ms
      ts <- mapM typecheckModule ps
      ds <- mapM desugarModule ts
      return $ map coreModule ds

