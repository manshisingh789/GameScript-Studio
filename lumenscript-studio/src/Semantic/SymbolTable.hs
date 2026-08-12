module Semantic.SymbolTable
  ( -- * The scoped variable table
    SymbolTable
  , SymbolInfo(..)
  , emptyTable
  , enterScope
  , exitScope
  , declare
  , addSymbol
  , lookupSymbol
  , toList

    -- * Built-in game objects (player / enemy / npc)
  , isBuiltinObject
  , lookupMember
  ) where

import qualified Data.Map as Map
import Data.Map (Map)

import Token (Position)
import Semantic.ErrorLog (Type(..))

toList :: SymbolTable -> [(String, SymbolInfo)]
toList (SymbolTable ss) = concatMap Map.toList ss

-- This module manages two distinct but related concerns:
-- 1. A stack of lexical scopes for user-declared variables.
-- 2. A simple, global map of built-in game objects and their members.


-- ---------------------------------------------------------------------------
-- User-declared variables
-- ---------------------------------------------------------------------------

-- | Information stored for a declared variable.
-- We keep both its type and the position of its original declaration so that
-- duplicate declaration errors can point back to the original.
data SymbolInfo = SymbolInfo
  { symbolType :: Type
  , symbolPos  :: Position
  }
  deriving (Eq, Show)


-- | A single lexical scope, mapping variable names to their info.
type Scope = Map String SymbolInfo


-- | A stack of lexical scopes. The head of the list is the innermost scope.
newtype SymbolTable = SymbolTable
  { scopes :: [Scope]
  }
  deriving (Eq, Show)


-- | Create a fresh symbol table containing only the global scope.
emptyTable :: SymbolTable
emptyTable = SymbolTable [Map.empty]


-- | Enter a new nested lexical scope (e.g., for an `if` block).
enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable ss) = SymbolTable (Map.empty : ss)


-- | Exit the current lexical scope. Does nothing if only the global scope remains.
exitScope :: SymbolTable -> SymbolTable
exitScope st@(SymbolTable [_]) = st
exitScope (SymbolTable (_ : rest)) = SymbolTable rest
exitScope st@(SymbolTable []) = st -- Should not happen, but is safe.


-- | Declare a variable in the current (innermost) scope.
-- This is used during the scope resolution pass. It prevents re-declaration
-- within the same scope but allows shadowing of variables from outer scopes.
declare
  :: String
  -> Type
  -> Position
  -> SymbolTable
  -> Either Position SymbolTable -- Using `Either` here is idiomatic for functions that can fail.
                                 -- `Left` carries an error value (the position of the existing symbol).
                                 -- `Right` carries the success value (the updated symbol table).
declare name ty pos (SymbolTable (current : rest)) =
  case Map.lookup name current of
    -- Already declared in this scope, return position of original declaration.
    Just existing -> Left (symbolPos existing)
    -- New declaration, add it to the current scope.
    Nothing ->
      let updatedScope = Map.insert name (SymbolInfo ty pos) current
      in Right (SymbolTable (updatedScope : rest))
declare _ _ _ (SymbolTable []) =
  error "SymbolTable.declare: empty scope stack -- this should be impossible."


-- | Add a new symbol or update an existing one in the current scope.
-- This is used by the type checker to fill in the real type of a declaration
-- that the scope resolution pass may have left as TUnknown.
addSymbol
  :: String
  -> Type
  -> Position
  -> SymbolTable
  -> SymbolTable
addSymbol name ty pos (SymbolTable (current : rest)) =
  let updated = Map.insert name (SymbolInfo ty pos) current
  in SymbolTable (updated : rest)
addSymbol _ _ _ (SymbolTable []) =
  error "SymbolTable.addSymbol: empty scope stack -- this should be impossible."


-- | Look up a variable from the current scope outward to the global scope.
-- The innermost matching declaration is returned, which provides lexical shadowing.
lookupSymbol
  :: String
  -> SymbolTable
  -> Maybe SymbolInfo -- Using `Maybe` is the standard Haskell way to represent a lookup
                      -- that may or may not find a result. `Nothing` means the symbol
                      -- was not found in any scope. `Just info` means it was found.
lookupSymbol name (SymbolTable ss) = go ss
  where
    go [] = Nothing
    go (scope : rest) =
      case Map.lookup name scope of
        Just info -> Just info
        Nothing   -> go rest


-- ---------------------------------------------------------------------------
-- Built-in game objects
-- ---------------------------------------------------------------------------

-- | A map of built-in object names to their members.
-- e.g., "player" -> {"jump" -> TFunction [] TVoid, "level" -> TInt}
type Builtins = Map String (Map String Type)


-- | The complete set of built-in objects and their members.
builtinObjects :: Builtins
builtinObjects =
  Map.fromList
    [ ( "player"
      , Map.fromList
          [ ("jump",     TFunction [] TVoid)
          , ("distance", TInt)
          , ("level",    TInt)
          ]
      )
    , ( "enemy"
      , Map.fromList
          [ ("attack", TFunction [] TVoid)
          , ("patrol", TFunction [] TVoid)
          ]
      )
    , ( "npc"
      , Map.fromList
          [ ("say", TFunction [TString] TVoid)
          ]
      )
    ]


-- | Check whether a name corresponds to a built-in game object.
isBuiltinObject :: String -> Bool
isBuiltinObject name = Map.member name builtinObjects


-- | Look up a member of a built-in object (e.g., "level" of "player").
-- Returns `Nothing` if either the object or the member does not exist.
lookupMember
  :: String
  -> String
  -> Maybe Type
lookupMember base field =
  Map.lookup base builtinObjects >>= Map.lookup field