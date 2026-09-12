module GUI.BytecodeViewer
  ( BytecodeViewer(..)
  , createBytecodeViewer
  , updateBytecodeViewer
  , formatCompiledProgram
  , dummyBytecodeLines
  ) where

import Graphics.UI.Threepenny.Core (Element, UI)
import Data.List (findIndex, sortBy)
import AST (EventSpec(..))
import Bytecode.Instruction (Instr(..))
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

-- | Render the flat bytecode stream as the two logical parts of a
-- CompiledProgram: the main block followed by one block per handler.
-- Each block gets local instruction numbers so the event-driven structure
-- is visible without changing the VM's flat instruction representation.
formatCompiledProgram :: CompiledProgram -> [String]
formatCompiledProgram prog =
  formatBlock "MAIN" mainInstrs ++ concatMap formatHandler orderedHandlers
  where
    instrs = cpInstrs prog
    handlers = cpHandlers prog
    handlerLabels = map snd handlers
    mainInstrs = takeWhile (not . isHandlerEntry handlerLabels) instrs
    orderedHandlers = sortBy (compareHandler instrs) handlers

    formatHandler (spec, label) =
      [""] ++ formatBlock ("HANDLER: " ++ eventName spec)
        (handlerInstructions label)

    formatBlock title block =
      [title, replicate (length title) '-'] ++ formatInstructions block

    formatInstructions [] = ["(no instructions)"]
    formatInstructions block =
      let padWidth = length (show (length block - 1))
          formatInstr (i, instr) =
            let indexStr = show i
                padding = replicate (padWidth - length indexStr) ' '
            in indexStr ++ padding ++ "  " ++ show instr
      in map formatInstr (zip [0..] block)

    isHandlerEntry labels (LABEL label) = label `elem` labels
    isHandlerEntry _ _ = False

    compareHandler allInstrs (_, firstLabel) (_, secondLabel) =
      compare (labelPosition allInstrs firstLabel)
              (labelPosition allInstrs secondLabel)

    labelPosition allInstrs label =
      maybe maxBound id (findIndex (isLabel label) allInstrs)

    isLabel label (LABEL actual) = label == actual
    isLabel _ _ = False

    handlerInstructions label =
      takeThroughBoundary (dropWhile (not . isLabel label) instrs)

    takeThroughBoundary [] = []
    takeThroughBoundary (instr : rest) =
      instr : if isAfterEvent instr
                then []
                else takeThroughBoundary rest

    isAfterEvent (LABEL label) = "after_event_" `isPrefixOf` label
    isAfterEvent _ = False

    isPrefixOf prefix value = take (length prefix) value == prefix

    eventName (KeyPress key) = key
    eventName (GenericEvent name _) = name

-- | Static placeholder rows shown before the first successful compile.
dummyBytecodeLines :: [String]
dummyBytecodeLines =
  [ "0  LABEL main"
  , "1  PUSH_INT 5"
  , "2  STORE_VAR x"
  , "3  LOAD player distance"
  , "4  CALL player jump 0"
  , "5  POP"
  ]