module Semantic
( analyzeProgram
, SemanticResult(..)
) where

import AST (Program)
import Semantic.ErrorLog (SemanticError)
import Semantic.ScopeResolution (resolveProgram)
import Semantic.TypeCheck (typeCheckProgram)

data SemanticResult = SemanticResult
  { semanticErrors :: [SemanticError]
  , semanticPassed :: Bool
  }
  deriving (Eq, Show)

analyzeProgram :: Program -> SemanticResult
analyzeProgram prog =
  let scopeErrors = resolveProgram prog
      typeErrors  = typeCheckProgram prog
      allErrors   = scopeErrors ++ typeErrors
  in SemanticResult
       { semanticErrors = allErrors
       , semanticPassed = null allErrors
       }