module Bytecode.Instruction where

import AST (BinOp, UnaryOp, Ident)

-- | A label is a string that marks a position in the bytecode.
type Label = String

-- | Represents a single bytecode instruction for the LumenScript VM.
data Instr
  = PUSH_INT Int        -- | Push an integer onto the stack.
  | PUSH_STR String     -- | Push a string onto the stack.
  | PUSH_BOOL Bool      -- | Push a boolean onto the stack.
  | LOAD Ident          -- | Push the value of a global variable onto the stack.
  | STORE Ident         -- | Pop a value from the stack and store it in a global variable.
  | LOAD_LOCAL Int      -- | Push the value of a local variable (by index) onto the stack.
  | STORE_LOCAL Int     -- | Pop a value from the stack and store it in a local variable (by index).
  | LOAD_MEMBER Ident   -- | Pop an object from the stack, push the value of its member.
  | STORE_MEMBER Ident  -- | Pop a value and an object from the stack, set the member on the object.
  | OP BinOp            -- | Pop two values, apply the binary operator, and push the result.
  | UOP UnaryOp         -- | Pop one value, apply the unary operator, and push the result.
  | JUMP Label          -- | Unconditionally jump to a label.
  | JUMP_IF_FALSE Label -- | Pop a value, if it's false, jump to a label.
  | LABEL Label         -- | A pseudo-instruction that marks a location in the code.
  | CALL Ident Int      -- | Call a function by name with a number of arguments.
  | POP                 -- | Remove the top item from the stack.
  | DUP                 -- | Duplicate the top item on the stack.
  | SWAP                -- | Swap the top two items on the stack.
  | RET                 -- | Return from the current function.
  | HALT                -- | Stop program execution.
  | NEW_ARRAY           -- | Create a new, empty array and push it onto the stack.
  | NEW_DICT            -- | Create a new, empty dictionary and push it onto the stack.
  | ARRAY_GET           -- | Pop an index and an array, push the array element at the index.
  | ARRAY_SET           -- | Pop a value, an index, and an array; set the element at the index.
  | DICT_GET            -- | Pop a key and a dictionary, push the value for the key.
  | DICT_SET            -- | Pop a value, a key, and a dictionary; set the value for the key.
  deriving (Show, Eq)

-- | Represents a fully compiled program, ready for execution by the VM.
data CompiledProgram = CompiledProgram
  { mainBlock :: [Instr] -- ^ The main sequence of instructions to execute.
  , handlers :: [(Label, [Instr])] -- ^ A map of event handler labels to their instruction sequences.
  } deriving (Show, Eq)