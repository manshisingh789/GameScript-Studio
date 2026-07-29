module Bytecode.Instruction where

import AST (Ident, CompOp)

type Label = String

data Instr
  = PUSH_INT Int
  | PUSH_STR String
  | LOAD Ident Ident            -- LOAD player distance
  | CMP CompOp
  | JUMP_IF_FALSE Label
  | JUMP Label
  | LABEL Label
  | CALL Ident Ident Int        -- CALL npc say 1   (arg count)
  deriving (Show, Eq)