module Token where

data Token
  = TInt Int
  | TString String
  | TBool Bool
  | TIdent String
  | TDot | TComma
  | TLParen | TRParen | TColon
  | TAssign
  | TPlus | TMinus | TMultiply | TDivide | TMod
  | TLt | TGt | TEq | TNotEqual | TLe | TGe
  | TKwLet | TKwIf | TKwElse | TKwOn | TKwKeyPress
  | TEOF
  deriving (Show, Eq)

keywords :: [(String, Token)]
keywords =
  [ ("let",   TKwLet)
  , ("if",    TKwIf)
  , ("else",  TKwElse)
  , ("on",    TKwOn)
  , ("key_press", TKwKeyPress)
  , ("true",  TBool True)
  , ("false", TBool False)
  ]
