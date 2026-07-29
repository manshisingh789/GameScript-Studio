module HostBindings where

import AST (Ident)

data MemberKind = Field | Method
  deriving (Show, Eq)

data LumType = TyInt | TyString
  deriving (Show, Eq)

-- (object, member, kind, argument types)
hostBindings :: [(Ident, Ident, MemberKind, [LumType])]
hostBindings =
  [ ("player", "distance", Field,  [])
  , ("player", "level",    Field,  [])
  , ("player", "jump",     Method, [])
  , ("enemy",  "attack",   Method, [])
  , ("enemy",  "patrol",   Method, [])
  , ("npc",    "say",      Method, [TyString])
  ]