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
import AST (Program, Stmt(..), Mutability(..), Ident)

data Symbol = Symbol
  { symName :: Ident
  , symIndex :: Int
  , symMutability :: Mutability
  } deriving (Show, Eq)

isGlobal :: SymbolTable -> Bool
isGlobal st = length (scopes st) == 1

fromProgram :: Program -> SymbolTable
fromProgram stmts =
  let
    globals = foldr collectGlobals [] stmts
    scopeMap = Map.fromList $ map (\s -> (symName s, s)) globals
    numLocals = length globals
  in SymbolTable [scopeMap] numLocals

collectGlobals :: Stmt -> [Symbol] -> [Symbol]
collectGlobals (Decl name mutability _ _) acc =
  let newIndex = length acc
  in Symbol name newIndex mutability : acc
collectGlobals _ acc = acc

-- A stack of scopes, where each scope maps a variable name to its Symbol.
-- The head of the list is the innermost scope.
data SymbolTable = SymbolTable
  { scopes     :: [Map String Symbol]
  , localCount :: Int -- Total number of locals allocated in this context.
  } deriving (Show, Eq)

-- | An empty symbol table with a single global scope.
empty :: SymbolTable
empty = SymbolTable [Map.empty] 0

-- | Define a new variable in the current scope.
-- It assigns the variable the next available local index.
define :: String -> Mutability -> SymbolTable -> (SymbolTable, Symbol)
define name mutability st@(SymbolTable (current:rest) count) =
  let newIndex = count
      newSymbol = Symbol name newIndex mutability
      newScope = Map.insert name newSymbol current
      newTable = st { scopes = newScope : rest, localCount = count + 1 }
  in (newTable, newSymbol)
define _ _ (SymbolTable [] _) = error "SymbolTable.define: empty scope stack"

-- | Resolve a variable name to its Symbol, searching from the
-- innermost scope outwards.
resolve :: String -> SymbolTable -> Maybe Symbol
resolve name (SymbolTable ss _) = go ss
  where
    go [] = Nothing
    go (scope:rest) =
      case Map.lookup name scope of
        Just symbol -> Just symbol
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