module Semantic.SymbolTable
  ( -- * The scoped variable table
    SymbolTable
  , SymbolInfo(..)
  , emptyTable
  , enterScope
  , exitScope
  , declare
  , lookupSymbol

    -- * Built-in game objects (player / enemy / npc)
  , isBuiltinObject
  , lookupMember
  ) where

import qualified Data.Map as Map
import Data.Map (Map)

import Token (Position)
import Semantic.ErrorLog (Type(..))


-- ---------------------------------------------------------------------------
-- User-declared variables
-- ---------------------------------------------------------------------------

-- | Information stored for a declared variable.
--
-- We keep both its type and the position of its original declaration.
-- The position is required so that duplicate declarations can report
-- where the original declaration occurred.
data SymbolInfo = SymbolInfo
  { symbolType :: Type
  , symbolPos  :: Position
  }
  deriving (Eq, Show)


-- | A single lexical scope.
type Scope = Map String SymbolInfo


-- | A stack of lexical scopes.
--
-- The first scope is always the innermost/current scope.
-- The last scope is the global scope.
--
-- Example:
--
--   [ currentScope
--   , parentScope
--   , globalScope
--   ]
newtype SymbolTable = SymbolTable
  { scopes :: [Scope]
  }
  deriving (Eq, Show)


-- | Create a fresh symbol table containing only the global scope.
emptyTable :: SymbolTable
emptyTable = SymbolTable [Map.empty]


-- | Enter a new nested lexical scope.
--
-- Used when entering:
--
--   if {...}
--   else {...}
--   on key_press ... {...}
--
enterScope :: SymbolTable -> SymbolTable
enterScope (SymbolTable ss) =
  SymbolTable (Map.empty : ss)


-- | Exit the current lexical scope.
--
-- We never remove the global scope. If exitScope is accidentally
-- called while only the global scope exists, it simply does nothing.
exitScope :: SymbolTable -> SymbolTable
exitScope st@(SymbolTable [_]) =
  st

exitScope (SymbolTable (_ : rest)) =
  SymbolTable rest

exitScope st@(SymbolTable []) =
  st


-- | Declare a variable in the current/innermost scope.
--
-- Returns:
--
--   Right updatedTable
--       when the variable does not already exist in the current scope.
--
--   Left originalDeclarationPosition
--       when the variable already exists in the current scope.
--
-- Variables in outer scopes do NOT cause an error here. This allows
-- shadowing:
--
--   let x = 10
--   if (...) {
--       let x = 20
--   }
--
declare
  :: String
  -> Type
  -> Position
  -> SymbolTable
  -> Either Position SymbolTable

declare name ty pos (SymbolTable (current : rest)) =
  case Map.lookup name current of

    -- Already declared in this scope.
    Just existing ->
      Left (symbolPos existing)

    -- New declaration.
    Nothing ->
      let updatedScope =
            Map.insert name (SymbolInfo ty pos) current
      in
        Right (SymbolTable (updatedScope : rest))

declare _ _ _ (SymbolTable []) =
  error
    "SymbolTable.declare: empty scope stack -- emptyTable always creates a global scope"


-- | Look up a variable from the current scope outward.
--
-- The innermost matching declaration wins, which gives us normal
-- lexical shadowing behaviour.
lookupSymbol
  :: String
  -> SymbolTable
  -> Maybe SymbolInfo

lookupSymbol name (SymbolTable ss) =
  go ss
  where
    go [] =
      Nothing

    go (scope : rest) =
      case Map.lookup name scope of
        Just info ->
          Just info

        Nothing ->
          go rest


-- ---------------------------------------------------------------------------
-- Built-in game objects
-- ---------------------------------------------------------------------------

-- | Built-in object table.
--
-- Each built-in object maps member names to their semantic types.
--
-- Examples:
--
--   player.distance :: Int
--   player.level    :: Int
--   player.jump     :: () -> Void
--
--   npc.say         :: String -> Void
type Builtins = Map String (Map String Type)


-- | Built-in objects available without a user declaration.
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


-- | Check whether a name is a built-in game object.
--
-- Examples:
--
--   isBuiltinObject "player" == True
--   isBuiltinObject "npc"    == True
--   isBuiltinObject "x"      == False
isBuiltinObject :: String -> Bool
isBuiltinObject name =
  Map.member name builtinObjects


-- | Look up a member of a built-in object.
--
-- Returns:
--
--   Just TInt
--       for scalar properties such as player.distance.
--
--   Just (TFunction [...] TVoid)
--       for methods such as npc.say.
--
--   Nothing
--       when either the object or member does not exist.
lookupMember
  :: String
  -> String
  -> Maybe Type

lookupMember base field =
  Map.lookup base builtinObjects
    >>= Map.lookup field