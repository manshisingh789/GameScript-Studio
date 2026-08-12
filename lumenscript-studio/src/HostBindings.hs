module HostBindings where

import AST (Ident)

-- | Represents the type of a member.
data MemberKind = Field | Method
  deriving (Show, Eq)

-- | Represents the type of a value in the LumenScript language.
data LumType = TyInt | TyString | TyBool | TyVoid
  deriving (Show, Eq)

-- | Defines the signature of a global function available to the script.
--   (function name, argument types, return type)
data GlobalFunction = GlobalFunction Ident [LumType] LumType
  deriving (Show, Eq)

-- | A list of all global functions provided by the host.
hostGlobalFunctions :: [GlobalFunction]
hostGlobalFunctions =
  [ GlobalFunction "print" [TyString] TyVoid
  , GlobalFunction "jump" [] TyVoid
  , GlobalFunction "dash" [] TyVoid
  ]

-- | Defines the signature of a member on a host-defined object.
--   (object name, member name, kind, argument types)
hostObjectMembers :: [(Ident, Ident, MemberKind, [LumType])]
hostObjectMembers =
  [ ("player", "distance", Field,  [])
  , ("player", "level",    Field,  [])
  , ("player", "jump",     Method, [])
  , ("enemy",  "attack",   Method, [])
  , ("enemy",  "patrol",   Method, [])
  , ("npc",    "say",      Method, [TyString])
  ]