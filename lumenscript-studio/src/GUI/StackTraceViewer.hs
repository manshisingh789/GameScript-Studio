module GUI.StackTraceViewer
  ( StackTraceViewer(..)
  , createStackTraceViewer
  , updateStackTraceViewer
  , formatLiveStackTrace
  , dummyStackTraceLines
  ) where

import Graphics.UI.Threepenny.Core (Element, UI)
import GUI.Panel (createListPanel, updateListPanel)
import VM.Stack (Value, stackValues)
import VM.VM (VMState(..))

-- | The stack-trace / execution-trace viewer panel. GUI.App feeds it
-- post-instruction VM snapshots while the real simulation is running,
-- followed by the final variables and LOAD/CALL trace.
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

formatLiveStackTrace :: VMState -> [String]
formatLiveStackTrace state =
  [ "IP: " ++ show (vmPC state)
  , ""
  , "Operand Stack"
  , "-------------"
  ] ++ map formatValue (stackValues (vmStack state))
  where
    formatValue :: Value -> String
    formatValue value = "[ " ++ show value ++ " ]"

placeholderMsg :: String
placeholderMsg = "(no run yet -- step or run a script to see VM state here)"

-- | Static placeholder rows shown before any run/step has happened.
dummyStackTraceLines :: [String]
dummyStackTraceLines =
  [ "PC: 4"
  , "Stack (top -> bottom): VInt 5, VStr \"hello\""
  , "Vars: x = VInt 5, y = VInt 0"
  ]