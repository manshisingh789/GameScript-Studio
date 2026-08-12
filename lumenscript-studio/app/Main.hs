module Main where

import System.Environment (getArgs)
import Compiler (compile, CompilationResult(..))

main :: IO ()
main = do
    args <- getArgs
    case args of
        [file] -> do
            source <- readFile file
            case compile source of
                CompilationSuccess compiledProgram -> do
                    putStrLn "Compilation Successful!"
                    print compiledProgram
                SemanticErrors errors -> do
                    putStrLn "Semantic Errors:"
                    mapM_ print errors
                ParseError err -> do
                    putStrLn "Parse Error:"
                    print err
        _ ->
            putStrLn "Usage: cabal run <script.lum>"