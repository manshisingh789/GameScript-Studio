module Compiler
    ( compile
    , CompilationResult(..)
    ) where

import qualified Lexer
import qualified Parser
import qualified Semantic
import qualified Bytecode.Generator as Generator
import AST (Program)
import VM.EventTrigger (CompiledProgram(..))
import Semantic (SemanticResult(..))
import Semantic.ErrorLog (SemanticError)

-- | The final result of a compilation attempt.
data CompilationResult
  = CompilationSuccess CompiledProgram
  | SemanticErrors [SemanticError]
  | ParseError String -- Using String for now, can be refined.
  deriving (Eq, Show)

-- | The main compilation pipeline.
--   Takes source text and returns either a compiled program or an error.
compile :: String -> CompilationResult
compile source =
  let tokens = Lexer.lexer source
  in case Parser.parseProgram tokens of
    Left err -> ParseError (show err) -- Representing parse error as a string
    Right (ast, _) ->
      let semanticResult = Semantic.analyzeProgram ast
      in if Semantic.semanticPassed semanticResult
           then case Generator.generateBytecode (Semantic.semanticAst semanticResult) of
                  Left genErr -> error ("Bytecode generation failed after semantic analysis passed: " ++ show genErr) -- This should not happen with a valid AST
                  Right program -> CompilationSuccess program
           else SemanticErrors (Semantic.semanticErrors semanticResult)