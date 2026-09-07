module GUI.BytecodeViewer
  ( BytecodeViewer(..)
  , createBytecodeViewer
  , updateBytecodeViewer
  , formatCompiledProgram
  , dummyBytecodeLines
  ) where

import Graphics.UI.Threepenny.Core (Element, UI)
import GUI.Panel (createListPanel, updateListPanel)
import VM.EventTrigger (CompiledProgram(..))

-- | The bytecode viewer panel. Wired to real data in GUI.App via
-- 'formatCompiledProgram' below, which lists handlers and
-- instructions one per line. CompiledProgram's own derived Show
-- renders the *whole record* -- including its entire instruction list
-- -- as a single unbroken line, which isn't readable as a bytecode
-- listing, so this module does its own formatting instead of using
-- GUI.Panel's generic `formatAsLines`. 'dummyBytecodeLines' below is
-- only the panel's placeholder before the very first compile.
data BytecodeViewer = BytecodeViewer
  { bvRoot    :: Element
  , bvContent :: Element
  }

createBytecodeViewer :: UI BytecodeViewer
createBytecodeViewer = do
  (root, content) <- createListPanel "Bytecode" placeholderMsg dummyBytecodeLines
  pure (BytecodeViewer root content)

updateBytecodeViewer :: BytecodeViewer -> [String] -> UI ()
updateBytecodeViewer bv = updateListPanel (bvContent bv) placeholderMsg

placeholderMsg :: String
placeholderMsg = "(no bytecode yet -- compile a script to see it here)"

-- | Render a CompiledProgram as a readable listing: its registered
-- event handlers (event spec -> entry label) first, then every
-- instruction on its own line, in the order Bytecode.Generator
-- emitted them. Instr already derives Show, so each line is just that
-- instruction's default rendering (e.g. "PUSH_INT 5").
formatCompiledProgram :: CompiledProgram -> [String]
formatCompiledProgram prog =
  handlerLines ++ [""] ++ instrLines
  where
    handlerLines
      | null (cpHandlers prog) = ["(no event handlers)"]
      | otherwise = "Handlers:" : map showHandler (cpHandlers prog)

    showHandler (spec, label) = "  " ++ show spec ++ " -> " ++ label

    instrLines = "Instructions:" : map (("  " ++) . show) (cpInstrs prog)

-- | Static placeholder rows shown before the first successful compile.
dummyBytecodeLines :: [String]
dummyBytecodeLines =
  [ "LABEL main"
  , "PUSH_INT 5"
  , "STORE_VAR x"
  , "LOAD player distance"
  , "CALL player jump 0"
  , "POP"
  ]