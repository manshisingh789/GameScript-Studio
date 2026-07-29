module Token where

data Token
  = TInt Int
  | TString String
  | TIdent String
  | TDot
  | TComma
  | TLParen
  | TRParen
  | TColon
  | TLt | TGt | TEq | TLe | TGe
  | TKwOn | TKwKeyPress | TKwIf | TKwElse
  | TEOF
  deriving (Show, Eq)