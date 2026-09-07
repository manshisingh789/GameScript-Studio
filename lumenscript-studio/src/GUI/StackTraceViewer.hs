module GUI.StackTraceViewer
  ( StackTraceViewer(..)
  , createStackTraceViewer
  , updateStackTraceViewer
  , dummyStackTraceLines
  ) where

import Graphics.UI.Threepenny.Core (Element, UI)
import GUI.Panel (createListPanel, updateListPanel)

-- | The stack-trace / execution-trace viewer panel. Shows
-- 'dummyStackTraceLines' only until the first compile -- GUI.App now
-- runs every registered event handler once through
-- Simulation.SimState against a real HostEnv/GridWorld, and calls
-- `updateStackTraceViewer viewer realTraceLines` with the resulting
-- variables and LOAD/CALL trace. That's a real run, but a post-run
-- summary rather than a live, single-instruction-at-a-time trace --
-- a true step-by-step view would need VM.VM to export its internal
-- stepping primitives (`step`, `findLabelIndex`, `handlerEnd`), which
-- it doesn't today.
data StackTraceViewer = StackTraceViewer
  { svRoot    :: Element
  , svContent :: Element
  }

createStackTraceViewer :: UI StackTraceViewer
createStackTraceViewer = do
  (root, content) <- createListPanel "Stack Trace" placeholderMsg dummyStackTraceLines
  pure (StackTraceViewer root content)

updateStackTraceViewer :: StackTraceViewer -> [String] -> UI ()
updateStackTraceViewer sv = updateListPanel (svContent sv) placeholderMsg

placeholderMsg :: String
placeholderMsg = "(no run yet -- step or run a script to see VM state here)"

-- | Static placeholder rows shown before any run/step has happened.
dummyStackTraceLines :: [String]
dummyStackTraceLines =
  [ "PC: 4"
  , "Stack (top -> bottom): VInt 5, VStr \"hello\""
  , "Vars: x = VInt 5, y = VInt 0"
  ]