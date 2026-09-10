module GUI.App (runGui) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Compiler (CompilationResult(..), PipelineStages(..), compileStages)
import VM.EventTrigger (CompiledProgram(..))
import Simulation.SimState
import VM.VM (VarEnv, VMState (vmVars), execLoop, defaultHostEnv, initVMState, handlerEnd)

import GUI.EditorPanel
import GUI.TokenViewer
import GUI.ASTViewer
import GUI.BytecodeViewer
import GUI.StackTraceViewer

-- | The script shown in the editor the very first time the server is
-- launched, before anyone has typed anything. Once you've edited the
-- box, 'runGui' below remembers your text (in memory, for as long as
-- the server process stays running) and pre-fills it on every
-- subsequent page load/refresh instead of falling back to this.
demoSource :: String
demoSource = unlines
  [ "let x = 5"
  , "on key_press SPACE: {"
  , "  player.jump()"
  , "}"
  ]

-- | Grid dimensions for the throwaway simulation world built each
-- time a script is run from the GUI. Arbitrary but fixed -- the GUI
-- doesn't (yet) expose a way to configure world size.
simWidth, simHeight :: Int
simWidth  = 10
simHeight = 8

-- | Launch the GUI: builds the editor and all four viewer panels,
-- lays them out on the page, and wires the "Compile" button to run
-- the full real pipeline exactly once per click via
-- Compiler.compileStages -- Editor -> Lexer -> Parser -> Semantic
-- Analyzer -> Bytecode Generator -> VM -> Simulation -> these panels,
-- with no stage re-run or duplicated work.
--
--   * Tokens      -- psTokens from compileStages: the real output of
--                    Lexer.lexer.
--   * AST         -- psParse from compileStages: the real output of
--                    Parser.parseProgram, shown independently of
--                    whether the later semantic/bytecode stages
--                    succeed, so you can see the AST even when
--                    semantic analysis reports an error.
--   * Bytecode    -- psResult's CompilationSuccess payload: real
--                    Bytecode.Generator output, rendered one handler
--                    and one instruction per line via
--                    GUI.BytecodeViewer.formatCompiledProgram. Cleared
--                    to a "no bytecode" message on a semantic or parse
--                    failure, rather than continuing to show whatever
--                    the previous successful compile produced.
--   * Stack Trace -- every event handler registered in the compiled
--                    program is actually run once, in order, through
--                    Simulation.SimState against a real
--                    HostEnv/GridWorld (real VM.VM.runEvent
--                    underneath). The panel shows the real final
--                    variables and the real LOAD/CALL trace. This is
--                    a post-run summary, not a live
--                    single-instruction stepper -- VM.VM doesn't
--                    currently export the per-instruction primitives
--                    (`step`, `findLabelIndex`, `handlerEnd`) that a
--                    "Step" button would need.
--
-- Persistence: 'savedSourceRef' is created exactly once, before
-- 'startGUI' is called -- not inside 'setup'. Threepenny calls 'setup'
-- fresh for every page load/refresh (each one is a new session), so
-- anything created inside 'setup' resets every time. By creating the
-- IORef outside it and capturing it in the closure passed to
-- 'startGUI', every session shares the same reference, so a refresh
-- reopens with whatever you last typed instead of snapping back to
-- 'demoSource'. This only lasts as long as the server process itself
-- keeps running -- stopping and relaunching `--gui` still starts
-- fresh, since nothing is written to disk.
runGui :: IO ()
runGui = do
  savedSourceRef <- newIORef demoSource
  savedVarEnvRef <- newIORef Map.empty
  startGUI defaultConfig (setup savedSourceRef savedVarEnvRef)

setup :: IORef String -> IORef VarEnv -> Window -> UI ()
setup savedSourceRef savedVarEnvRef window = do
  void $ return window # set UI.title "LumenScript Studio"

  initialSource <- liftIO $ readIORef savedSourceRef

  editor           <- createEditorPanel initialSource
  tokenViewer      <- createTokenViewer
  astViewer        <- createASTViewer
  bytecodeViewer   <- createBytecodeViewer
  stackTraceViewer <- createStackTraceViewer

  void $ getBody window #+
    [ element (epRoot editor)
    , element (tvRoot tokenViewer)
    , element (avRoot astViewer)
    , element (bvRoot bytecodeViewer)
    , element (svRoot stackTraceViewer)
    ]

  -- Save the textarea's content into 'savedSourceRef' whenever it
  -- changes (fires on blur/edit-commit, same as a normal HTML
  -- "change" event) -- not just when Compile is clicked -- so a
  -- refresh right after typing, without clicking Compile first,
  -- still keeps your text.
  on UI.valueChange (epSourceArea editor) $ \_ -> do
    current <- get UI.value (epSourceArea editor)
    liftIO $ writeIORef savedSourceRef current

  on UI.click (epCompileButton editor) $ \_ -> do
    source <- get UI.value (epSourceArea editor)
    liftIO $ writeIORef savedSourceRef source

    -- One pipeline run drives every panel below -- no separate
    -- Lexer/Parser calls, no duplicated work.
    let stages = compileStages source

    updateTokenViewer tokenViewer (map show (psTokens stages))

    case psParse stages of
      Right (program, _leftoverTokens) ->
        updateASTViewer astViewer (map show program)
      Left parseErr ->
        updateASTViewer astViewer ["Parse failed: " ++ show parseErr]

    case psResult stages of
      CompilationSuccess compiledProgram -> do
        void $ element (epStatus editor) # set UI.text "Compiled successfully."
        updateBytecodeViewer bytecodeViewer (formatCompiledProgram compiledProgram)
        
        -- Run the main script body and save the resulting variable environment
        let initialVarEnv = Map.empty
        finalVarEnv <- liftIO $ runMain compiledProgram initialVarEnv
        liftIO $ writeIORef savedVarEnvRef finalVarEnv
        
        traceLines <- runAllHandlers compiledProgram finalVarEnv
        updateStackTraceViewer stackTraceViewer traceLines

      SemanticErrors errs -> do
        void $ element (epStatus editor) # set UI.text
          ("Semantic errors:\n" ++ unlines (map show errs))
        updateBytecodeViewer bytecodeViewer
          ["(compile failed -- no bytecode)"]
        updateStackTraceViewer stackTraceViewer
          ["(compile failed -- nothing to run)"]

      ParseError err -> do
        void $ element (epStatus editor) # set UI.text
          ("Parse error: " ++ err)
        updateBytecodeViewer bytecodeViewer
          ["(compile failed -- no bytecode)"]
        updateStackTraceViewer stackTraceViewer
          ["(compile failed -- nothing to run)"]

-- | Actually run every event handler registered in the compiled
-- program once, in the order Bytecode.Generator recorded them,
-- against a fresh simulation world -- a real VM run through
-- Simulation.SimState.stepEvent, not fabricated data. Reports which
-- handlers ran, the final variable store, and the full LOAD/CALL
-- trace collected along the way.
runAllHandlers :: CompiledProgram -> VarEnv -> UI [String]
runAllHandlers prog initialVarEnv = liftIO $ do
  ref <- newIORef (initSimState simWidth simHeight initialVarEnv)
  let specs = map fst (cpHandlers prog)
  runQueued ref prog specs
  finalState <- readIORef ref
  pure $
    [ "Ran " ++ show (length specs) ++ " event handler(s): " ++ show specs
    , ""
    , "Final vars: " ++ show (Map.toList (simVars finalState))
    , ""
    , "Execution trace (oldest first):"
    ] ++ reverse (simLog finalState)

runMain :: CompiledProgram -> VarEnv -> IO VarEnv
runMain prog varEnv = do
    let st0 = initVMState (cpInstrs prog) varEnv 0
    stFinal <- execLoop defaultHostEnv (handlerEnd "" (cpInstrs prog) []) st0
    pure (vmVars stFinal)