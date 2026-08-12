module Semantic
( analyzeProgram
, SemanticResult(..)
, SymbolTable
) where

import AST (Program)
import Semantic.ErrorLog (SemanticError)
import Semantic.ScopeResolution (resolveProgram)
import Semantic.TypeCheck (typeCheckProgram)
import Semantic.SymbolTable (SymbolTable)

data SemanticResult = SemanticResult
  { semanticErrors :: [SemanticError]
  , semanticPassed :: Bool
  , semanticAst    :: Program
  , semanticSymbolTable :: SymbolTable
  }
  deriving (Eq, Show)

analyzeProgram :: Program -> SemanticResult
analyzeProgram prog =
  let (scopeErrors, finalTable) = resolveProgram prog
      typeErrors  = typeCheckProgram prog
      allErrors   = scopeErrors ++ typeErrors
  in SemanticResult
       { semanticErrors = allErrors
       , semanticPassed = null allErrors
       , semanticAst    = prog
       , semanticSymbolTable = finalTable
       }