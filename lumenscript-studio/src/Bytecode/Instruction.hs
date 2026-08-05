module Bytecode.Instruction where

import AST (Ident, BinOp)

type Label = String

data Instr
  = PUSH_INT Int
  | PUSH_STR String
  | LOAD Ident Ident            -- LOAD player distance
  | CMP BinOp
  | JUMP_IF_FALSE Label
  | JUMP Label
  | LABEL Label
  | CALL Ident Ident Int        -- CALL npc say 1   (arg count)
  deriving (Show, Eq)