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
  | LOAD_LOCAL Int
  | STORE_LOCAL Int
  | LOAD_MEMBER Ident
  | STORE_MEMBER Ident
  | OP BinOp
  | UOP UnaryOp
  | JUMP Label
  | JUMP_IF_FALSE Label
  | LABEL Label
  | CALL Ident Int -- method name, arg count
  | POP          -- Remove top of stack
  | DUP          -- Duplicate top of stack
  | SWAP         -- Swap top two stack elements
  | RET          -- Return from function
  | HALT         -- Stop program execution
  | NEW_ARRAY    -- Create new array
  | NEW_DICT     -- Create new dictionary
  | ARRAY_GET    -- Get array element
  | ARRAY_SET    -- Set array element
  | DICT_GET     -- Get dict value
  | DICT_SET     -- Set dict value
  deriving (Show, Eq)

data CompiledProgram = CompiledProgram
  { mainBlock :: [Instr]
  , handlers :: [(Label, [Instr])]
  } deriving (Show, Eq)