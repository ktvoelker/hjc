
{-# LANGUAGE TupleSections #-}
module Main where

-- Mostly taken from http://www.haskell.org/haskellwiki/GHC/As_a_library

import qualified DynFlags as DF
import HscTypes (ModGuts)
import GHC
import GHC.Paths (libdir)

import Control.Monad
import Data.Maybe
import qualified System.Console.GetOpt as GO
import System.Environment (getArgs)

import Compiler
import Output

usage = putStrLn $ GO.usageInfo "hjc" options

data Option =
    Dump
  | Output
  | Input
  | Natives
  | MainFunc
  deriving (Bounded, Enum, Eq, Ord, Read, Show)

type OptionWithArg = (Option, Maybe String)

noArg opt = GO.NoArg (opt, Nothing)

reqArg opt desc = flip GO.ReqArg desc $ (opt,) . Just

optArg opt desc = GO.ReqArg (opt,) desc

options =
  [ GO.Option "d" ["dump"] (noArg Dump) "dump CoreSyn structure"
  , GO.Option "o" ["output"] (reqArg Output "FILE") "output"
  , GO.Option "i" ["input"] (reqArg Input "FILE") "input"
  , GO.Option "n" ["natives"] (reqArg Natives "FILE") "natives"
  , GO.Option "m" ["main"] (reqArg MainFunc "MODULE.NAME") "main function"
  ]

optionArg o = join . lookup o

hasOption o = isJust . lookup o
 
main :: IO ()
main = do
  args <- getArgs
  let (opts, others, errors) = GO.getOpt GO.RequireOrder options args
  case errors of
    [] -> case optionArg Input opts of
      Nothing -> usage
      Just inputFile -> do
        mods <- getModules inputFile
        when (hasOption Dump opts) $ do
          return () -- TODO dump
        case optionArg Output opts of
          Nothing -> return ()
          Just outputFile ->
            writeModules (optionArg Natives opts) outputFile
            $ map (compileModule $ optionArg MainFunc opts) mods
    _ -> usage
 
getModules :: String -> IO [ModGuts]
getModules targetName = 
  defaultErrorHandler DF.defaultLogAction $ do
    runGhc (Just libdir) $ do
      dflags <- getSessionDynFlags
      let dflags' = foldl DF.xopt_set dflags
                          [DF.Opt_Cpp, DF.Opt_ImplicitPrelude, DF.Opt_MagicHash]
      setSessionDynFlags dflags'
      target <- guessTarget targetName Nothing
      setTargets [target]
      load LoadAllTargets
      ms <- getModuleGraph
      ps <- mapM parseModule ms
      ts <- mapM typecheckModule ps
      ds <- mapM desugarModule ts
      return $ map coreModule ds

