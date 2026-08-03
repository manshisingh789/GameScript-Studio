module Token where

data Position = Position { line :: Int, column :: Int }
  deriving (Show, Eq)

data TokenPayload
  = TInt Int
  | TString String
  | TBool Bool
  | TIdent String
  | TDot | TComma | TColon | TSemicolon
  | TLParen | TRParen | TBraceL | TBraceR | TBracketL | TBracketR
  | TAssign
  | TPlus | TMinus | TMultiply | TDivide | TMod
  | TLt | TGt | TEq | TNotEqual | TLe | TGe
  | TKwLet | TKwIf | TKwElse | TKwElif | TKwOn | TKwKeyPress | TKwCollision | TKwUpdate | TKwInteract
  | TIndent | TDedent
  | TNewline
  | TEOF
  | TError String
  deriving (Show, Eq)

data Token = Token
  { tokenType    :: TokenPayload
  , tokenPosition :: Position
  } deriving (Show, Eq)

keywords :: [(String, TokenPayload)]
keywords =
  [ ("let",   TKwLet)
  , ("if",    TKwIf)
  , ("else",  TKwElse)
  , ("elif",  TKwElif)
  , ("on",    TKwOn)
  , ("key_press", TKwKeyPress)
  , ("collision", TKwCollision)
  , ("update", TKwUpdate)
  , ("interact", TKwInteract)
  , ("true",  TBool True)
  , ("false", TBool False)
  ]