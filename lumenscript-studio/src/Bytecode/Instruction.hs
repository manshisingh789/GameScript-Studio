module Bytecode.Instruction where

import AST (Ident, BinOp, UnaryOp)
import Control.DeepSeq (NFData(..))

type Label = String

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

instance NFData Instr where
    rnf instr = case instr of
        PUSH_INT n -> rnf n
        PUSH_STR s -> rnf s
        PUSH_BOOL b -> rnf b
        LOAD a b -> rnf a `seq` rnf b
        LOAD_VAR name -> rnf name
        STORE_VAR name -> rnf name
        BINOP op -> rnf op
        UNOP op -> rnf op
        JUMP_IF_FALSE label -> rnf label
        JUMP label -> rnf label
        LABEL label -> rnf label
        CALL a b n -> rnf a `seq` rnf b `seq` rnf n
        POP -> ()