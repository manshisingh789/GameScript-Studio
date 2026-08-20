module Bytecode.SymbolTable
  ( SymbolTable
  , empty
  , define
  , resolve
  , enterScope
  , exitScope
  , localCount
  , fromProgram
  , isGlobal
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (foldl')
import AST (Program, Stmt(..))

isGlobal :: SymbolTable -> Bool
isGlobal st = length (scopes st) == 1

fromProgram :: Program -> SymbolTable
fromProgram stmts =
  let
    globalNames = foldr collectGlobals [] stmts
    scopeMap = Map.fromList $ zip globalNames [0..]
    numLocals = length globalNames
  in SymbolTable [scopeMap] numLocals

collectGlobals :: Stmt -> [String] -> [String]
collectGlobals (Decl name _ _) acc = name : acc
collectGlobals _ acc = acc

-- A stack of scopes, where each scope maps a variable name to its stack index.
-- The head of the list is the innermost scope.
data SymbolTable = SymbolTable
  { scopes     :: [Map String Int]
  , localCount :: Int -- Total number of locals allocated in this context.
  } deriving (Show, Eq)

-- | An empty symbol table with a single global scope.
empty :: SymbolTable
empty = SymbolTable [Map.empty] 0

-- | Define a new variable in the current scope.
-- It assigns the variable the next available local index.
define :: String -> SymbolTable -> (SymbolTable, Int)
define name st@(SymbolTable (current:rest) count) =
  let newIndex = count
      newScope = Map.insert name newIndex current
      newTable = st { scopes = newScope : rest, localCount = count + 1 }
  in (newTable, newIndex)
define _ (SymbolTable [] _) = error "SymbolTable.define: empty scope stack"

-- | Resolve a variable name to its stack index, searching from the
-- innermost scope outwards.
resolve :: String -> SymbolTable -> Maybe Int
resolve name (SymbolTable ss _) = go ss
  where
    go [] = Nothing
    go (scope:rest) =
      case Map.lookup name scope of
        Just index -> Just index
        Nothing    -> go rest

-- | Enter a new, nested lexical scope.
enterScope :: SymbolTable -> SymbolTable
enterScope st = st { scopes = Map.empty : scopes st }

-- | Exit the current scope, discarding all variables defined within it.
-- The total local count remains, as stack slots are not reclaimed within
-- the current function body.
exitScope :: SymbolTable -> SymbolTable
exitScope st@(SymbolTable [_] _) = st -- Cannot exit global scope
exitScope st@(SymbolTable (_:rest) _) = st { scopes = rest }
exitScope (SymbolTable [] _) = error "SymbolTable.exitScope: empty scope stack"