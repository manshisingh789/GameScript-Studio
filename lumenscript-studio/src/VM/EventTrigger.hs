module VM.EventTrigger
  ( CompiledProgram(..)
  , findHandler
  ) where

import AST (EventSpec(..))
import Bytecode.Instruction (Instr, Label)

-- | The compiled form of a whole Program: one flat instruction stream (every
-- OnEvent body concatenated back to back, each preceded by a LABEL marking
-- its entry point) plus a table saying which EventSpec maps to which entry
-- label. Generator.hs is expected to produce this from a [Stmt] full of
-- OnEvent blocks.
data CompiledProgram = CompiledProgram
  { cpInstrs   :: [Instr]
  , cpHandlers :: [(EventSpec, Label)]
  } deriving (Show, Eq)

-- | Look up which label to jump to when `spec` fires. If two `on key_press
-- SPACE:` blocks exist, this returns the first match -- Semantic checking
-- should reject duplicate handlers for the same event before we get here,
-- so the VM doesn't need to pick between them.
findHandler :: EventSpec -> CompiledProgram -> Maybe Label
findHandler spec (CompiledProgram _ handlers) = lookup spec handlers