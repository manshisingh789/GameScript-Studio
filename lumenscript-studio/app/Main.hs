module Main where

import System.Environment (getArgs)

import Lexer
import Parser

main :: IO ()
main = do
    args <- getArgs

    case args of

        [file] -> do

            source <- readFile file

            let tokens = lexer source

            putStrLn "========== TOKENS =========="
            mapM_ print tokens

            putStrLn "\n========== PARSER =========="

            case parseProgram tokens of

                Left err ->
                    print err

                Right (ast, _) -> do

                    putStrLn "Parse Successful!\n"

                    print ast

        _ ->
            putStrLn "Usage: stack run <script.lum>"