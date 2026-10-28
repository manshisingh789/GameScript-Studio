-- | Terminal view of the error pipeline, using the REAL compiler:
--
--   stack runghc -- -isrc -itest test/ErrorDemo.hs demos/errors/01_parse_error.lum
module Main (main) where

import System.Environment (getArgs)

import Compiler (CompilationResult(..), PipelineStages(..), compileStages)
import ErrorReport

main :: IO ()
main = do
  args <- getArgs
  case args of
    [path] -> do
      src <- readFile path
      putStrLn ("File: " ++ path)
      mapM_ putStrLn (renderPlain (buildReport (outcomeOf src (compileStages src))))
    _ -> putStrLn "usage: stack runghc -- -isrc -itest test/ErrorDemo.hs <file.lum>"

outcomeOf :: String -> PipelineStages -> PipelineOutcome
outcomeOf src stages = PipelineOutcome
  { poSource         = src
  , poTokens         = psTokens stages
  , poParseError     = either (Just . show) (const Nothing) (psParse stages)
  , poSemanticErrors = case psResult stages of SemanticErrors es -> map show es; _ -> []
  , poCompiled       = case psResult stages of CompilationSuccess _ -> True; _ -> False
  }