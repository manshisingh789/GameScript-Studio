module Bytecode.Instruction where

import AST (Ident, BinOp, UnaryOp)

-- | A label is a string that marks a position in the bytecode.
type Label = String

-- | Represents a single bytecode instruction for the LumenScript VM.
data Instr
  = PUSH_INT Int
  | PUSH_STR String
  | PUSH_BOOL Bool
  | LOAD Ident Ident            -- LOAD player distance
  | LOAD_VAR Ident              -- LOAD_VAR x            (local variable read)
  | STORE_VAR Ident             -- STORE_VAR x           (local variable write)
  | BINOP BinOp                 -- BINOP Add / BINOP Lt / etc. (renamed from CMP,
                                 -- since it's used for arithmetic too, not just comparisons)
  | UNOP UnaryOp                -- UNOP Neg / UNOP Not
  | JUMP_IF_FALSE Label
  | JUMP Label
  | LABEL Label
  | CALL Ident Ident Int        -- CALL npc say 1   (arg count)
  | POP                         -- discard a bare expression-statement's result
  deriving (Show, Eq)
