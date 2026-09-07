module Compiler
    ( compile
    , CompilationResult(..)
    , compileStages
    , PipelineStages(..)
    ) where

import qualified Lexer
import qualified Parser
import qualified Semantic
import qualified Bytecode.Generator as Generator
import AST (Program)
import Token (Token)
import qualified ParserTypes as PT
import VM.EventTrigger (CompiledProgram(..))
import Semantic (SemanticResult(..))
import Semantic.ErrorLog (SemanticError)

-- | The final result of a compilation attempt.
data CompilationResult
  = CompilationSuccess CompiledProgram
  | SemanticErrors [SemanticError]
  | ParseError String -- Using String for now, can be refined.
  deriving (Eq, Show)

-- | Every stage's real intermediate result from a single pipeline
-- run, for callers (like the GUI) that want to show more than just
-- the final CompilationResult -- e.g. the real token stream and the
-- real parsed AST, even when a later stage fails or hasn't run.
--
-- 'compile' below is defined in terms of 'compileStages', so there is
-- exactly one implementation of the pipeline, not a second one
-- reimplemented by whatever displays these intermediate stages.
data PipelineStages = PipelineStages
  { psTokens :: [Token]
    -- ^ Real output of Lexer.lexer. Always present -- lexing can't
    -- fail outright, bad characters just become TError tokens in the
    -- stream.
  , psParse  :: Either PT.ParseError (Program, [Token])
    -- ^ Real output of Parser.parseProgram on 'psTokens'.
  , psResult :: CompilationResult
    -- ^ The same value 'compile' returns: derived from 'psParse',
    -- then Semantic analysis and Bytecode generation if parsing
    -- succeeded.
  }

-- | Run the full pipeline once, keeping every stage's real
-- intermediate result.
compileStages :: String -> PipelineStages
compileStages source =
  let tokens      = Lexer.lexer source
      parseResult = Parser.parseProgram tokens
      result = case parseResult of
        Left err -> ParseError (show err) -- Representing parse error as a string
        Right (ast, _) ->
          let semanticResult = Semantic.analyzeProgram ast
          in if Semantic.semanticPassed semanticResult
               then case Generator.generateBytecode (Semantic.semanticAst semanticResult) of
                      Left genErr -> error ("Bytecode generation failed after semantic analysis passed: " ++ show genErr) -- This should not happen with a valid AST
                      Right program -> CompilationSuccess program
               else SemanticErrors (Semantic.semanticErrors semanticResult)
  in PipelineStages tokens parseResult result

-- | The main compilation pipeline.
--   Takes source text and returns either a compiled program or an error.
compile :: String -> CompilationResult
compile = psResult . compileStages