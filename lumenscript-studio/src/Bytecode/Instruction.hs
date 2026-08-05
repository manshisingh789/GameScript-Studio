module Bytecode.Instruction where

import AST (BinOp, UnaryOp, Ident)

type Label = String
type Define = String

data Instr
  = PUSH_INT Int
  | PUSH_STR String
  | PUSH_BOOL Bool
  | LOAD Ident
  | STORE Ident
  | LOAD_MEMBER Ident
  | STORE_MEMBER Ident
  | OP BinOp
  | UOP UnaryOp
  | JUMP Label
  | JUMP_IF_FALSE Label
  | LABEL Label
  | CALL Ident Int -- method name, arg count
  deriving (Show, Eq)

data CompiledProgram = CompiledProgram
  { mainBlock :: [Instr]
  , handlers :: [(Label, [Instr])]
  } deriving (Show, Eq)