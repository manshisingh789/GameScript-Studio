module Token where

import Control.DeepSeq (NFData(..))

data Position = Position { line :: Int, column :: Int }
  deriving (Show, Eq)

instance NFData Position where
    rnf (Position l c) = rnf l `seq` rnf c

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
  | TNot
  | TKwLet | TKwConst | TKwIf | TKwElse | TKwElif | TKwOn | TKwKeyPress | TKwCollision | TKwUpdate | TKwInteract
  | TNewline
  | TBlank
  | TEOF
  | TError String
  deriving (Show, Eq)

instance NFData TokenPayload where
    rnf payload = case payload of
        TInt n       -> rnf n
        TString s    -> rnf s
        TBool b      -> rnf b
        TIdent s     -> rnf s
        TDot         -> ()
        TComma        -> ()
        TColon        -> ()
        TSemicolon   -> ()
        TLParen      -> ()
        TRParen      -> ()
        TBraceL      -> ()
        TBraceR      -> ()
        TBracketL    -> ()
        TBracketR    -> ()
        TAssign      -> ()
        TPlus        -> ()
        TMinus       -> ()
        TMultiply    -> ()
        TDivide      -> ()
        TMod          -> ()
        TLt           -> ()
        TGt           -> ()
        TEq           -> ()
        TNotEqual    -> ()
        TLe           -> ()
        TGe           -> ()
        TNot          -> ()
        TKwLet       -> ()
        TKwConst     -> ()
        TKwIf        -> ()
        TKwElse      -> ()
        TKwElif      -> ()
        TKwOn        -> ()
        TKwKeyPress  -> ()
        TKwCollision -> ()
        TKwUpdate    -> ()
        TKwInteract  -> ()
        TNewline     -> ()
        TBlank       -> ()
        TEOF         -> ()
        TError s     -> rnf s

data Token = Token
  { tokenType    :: TokenPayload
  , tokenPosition :: Position
  } deriving (Show, Eq)

instance NFData Token where
    rnf (Token token pos) = rnf token `seq` rnf pos

keywords :: [(String, TokenPayload)]
keywords =
  [ ("let",   TKwLet)
  , ("const", TKwConst)
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