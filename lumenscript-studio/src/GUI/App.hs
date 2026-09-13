module GUI.App (runGui) where

import Control.Concurrent (forkIO)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.Char (chr, toUpper)
import Data.List (find)
import Data.IORef
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import qualified Graphics.UI.Threepenny.Timer as UITimer

import AST (EventSpec(..))
import Compiler (CompilationResult(..), PipelineStages(..), compileStages)
import VM.EventTrigger (CompiledProgram(..))
import Simulation.SimState
import VM.VM (VarEnv, VMState(..), execLoopWithTrace, defaultHostEnv, initVMState, handlerEnd)

import GUI.EditorPanel
import GUI.TokenViewer
import GUI.ASTViewer
import GUI.BytecodeViewer
import GUI.StackTraceViewer
import GUI.SimulationViewer

data TraceUpdate
  = LiveFrame VMState
  | SimUpdate SimState
  | FinalFrame [String]

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
--   * Stack Trace -- post-instruction VM snapshots are queued while
--                    the main body and every event handler run through
--                    Simulation.SimState against a real HostEnv/GridWorld.
--                    The panel displays the live IP and operand stack,
--                    then the final variables and LOAD/CALL trace.
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
  let guiConfig = defaultConfig { jsStatic = Just "www" }
  startGUI guiConfig (setup savedSourceRef savedVarEnvRef)

setup :: IORef String -> IORef VarEnv -> Window -> UI ()
setup savedSourceRef savedVarEnvRef window = do
  void $ return window # set UI.title "LumenScript Studio"
  UI.addStyleSheet window "/ast.css"

  initialSource <- liftIO $ readIORef savedSourceRef

  editor           <- createEditorPanel initialSource
  tokenViewer      <- createTokenViewer (epSourceArea editor)
  astViewer        <- createASTViewer
  bytecodeViewer   <- createBytecodeViewer
  stackTraceViewer <- createStackTraceViewer
  simulationViewer <- createSimulationViewer
  traceQueueRef <- liftIO $ newIORef []
  compiledProgramRef <- liftIO $ newIORef Nothing
  simStateRef <- liftIO $ newIORef (initSimState simWidth simHeight Map.empty)

  timerAction <- UI.timer
  void $ pure timerAction # set UITimer.interval 16
  on UITimer.tick timerAction $ \_ -> do
    next <- liftIO $ atomicModifyIORef' traceQueueRef popTraceUpdate
    case next of
      Nothing -> pure ()
      Just (LiveFrame state) ->
        updateStackTraceViewer stackTraceViewer (formatLiveStackTrace state)
      Just (SimUpdate state) ->
        updateSimulationViewer simulationViewer state
      Just (FinalFrame lines') ->
        updateStackTraceViewer stackTraceViewer lines'
  UITimer.start timerAction

  body <- getBody window
  void $ pure body #+
    [ element (epRoot editor)
    , element (tvRoot tokenViewer)
    , element (avRoot astViewer)
    , element (bvRoot bytecodeViewer)
    , element (svRoot stackTraceViewer)
    , element (simRoot simulationViewer)
    ]

  -- Save the textarea's content into 'savedSourceRef' whenever it
  -- changes (fires on blur/edit-commit, same as a normal HTML
  -- "change" event) -- not just when Compile is clicked -- so a
  -- refresh right after typing, without clicking Compile first,
  -- still keeps your text.
  on UI.valueChange (epSourceArea editor) $ \_ -> do
    current <- get UI.value (epSourceArea editor)
    liftIO $ writeIORef savedSourceRef current

  let triggerKey keyCode = do
        compiled <- liftIO $ readIORef compiledProgramRef
        case compiled of
          Nothing -> pure ()
          Just prog ->
            case browserKeyName keyCode >>= matchingKeyName prog of
              Nothing -> pure ()
              Just keyName -> do
                liftIO $ stepEventWithTrace simStateRef prog (KeyPress keyName)
                  (enqueueTraceUpdate traceQueueRef . LiveFrame)
                currentState <- liftIO $ readIORef simStateRef
                liftIO $ enqueueTraceUpdate traceQueueRef (SimUpdate currentState)

  on UI.keydown body triggerKey

  on UI.click (epCompileButton editor) $ \_ -> do
    source <- get UI.value (epSourceArea editor)
    liftIO $ writeIORef savedSourceRef source

    -- One pipeline run drives every panel below -- no separate
    -- Lexer/Parser calls, no duplicated work.
    let stages = compileStages source

    updateTokenViewer tokenViewer (psTokens stages) (epSourceArea editor)

    case psParse stages of
      Right (program, _leftoverTokens) ->
        updateASTViewer astViewer (Right program)
      Left parseErr ->
        updateASTViewer astViewer (Left $ "Parse failed: " ++ show parseErr)

    case psResult stages of
      CompilationSuccess compiledProgram -> do
        void $ element (epStatus editor) # set UI.text "Compiled successfully."
        updateBytecodeViewer bytecodeViewer (formatCompiledProgram compiledProgram)
        liftIO $ atomicWriteIORef traceQueueRef []
        liftIO $ atomicWriteIORef compiledProgramRef Nothing
        updateStackTraceViewer stackTraceViewer ["IP: starting", "", "Operand Stack", "-------------"]
        void $ liftIO $ forkIO $ do
          let initialVarEnv = Map.empty
          finalVarEnv <- runMain compiledProgram initialVarEnv (enqueueTraceUpdate traceQueueRef . LiveFrame)
          writeIORef savedVarEnvRef finalVarEnv
          let initialSimState = initSimState simWidth simHeight finalVarEnv
          writeIORef simStateRef initialSimState
          writeIORef compiledProgramRef (Just compiledProgram)
          enqueueTraceUpdate traceQueueRef (SimUpdate initialSimState)

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
runAllHandlers :: CompiledProgram -> VarEnv -> (TraceUpdate -> IO ()) -> IO [String]
runAllHandlers prog initialVarEnv sendTrace = do
  ref <- newIORef (initSimState simWidth simHeight initialVarEnv)
  let specs = map fst (cpHandlers prog)
      trace = sendTrace . LiveFrame
  
  -- Send initial state before any handlers run
  initialSimState <- readIORef ref
  sendTrace (SimUpdate initialSimState)

  mapM_ (\spec -> do
    stepEventWithTrace ref prog spec trace
    currentSimState <- readIORef ref
    sendTrace (SimUpdate currentSimState)
    ) specs
  
  finalState <- readIORef ref
  pure $
    [ "Ran " ++ show (length specs) ++ " event handler(s): " ++ show specs
    , ""
    , "Final vars: " ++ show (Map.toList (simVars finalState))
    , ""
    , "Execution trace (oldest first):"
    ] ++ reverse (simLog finalState)

runMain :: CompiledProgram -> VarEnv -> (VMState -> IO ()) -> IO VarEnv
runMain prog varEnv trace = do
  let st0 = initVMState (cpInstrs prog) varEnv 0
  (stFinal, ()) <- execLoopWithTrace defaultHostEnv (handlerEnd "" (cpInstrs prog) []) () trace st0
  pure (vmVars stFinal)

enqueueTraceUpdate :: IORef [TraceUpdate] -> TraceUpdate -> IO ()
enqueueTraceUpdate ref update = atomicModifyIORef' ref (\updates -> (updates ++ [update], ()))

popTraceUpdate :: [TraceUpdate] -> ([TraceUpdate], Maybe TraceUpdate)
popTraceUpdate [] = ([], Nothing)
popTraceUpdate (update:updates) = (updates, Just update)

browserKeyName :: Int -> Maybe String
browserKeyName keyCode
  | keyCode >= 65 && keyCode <= 90 = Just [chr keyCode]
  | keyCode >= 48 && keyCode <= 57 = Just [chr keyCode]
  | keyCode >= 112 && keyCode <= 123 = Just ("F" ++ show (keyCode - 111))
  | otherwise = lookup keyCode
      [ (8, "BACKSPACE")
      , (9, "TAB")
      , (13, "ENTER")
      , (16, "SHIFT")
      , (17, "CTRL")
      , (18, "ALT")
      , (27, "ESCAPE")
      , (32, "SPACE")
      , (33, "PAGE_UP")
      , (34, "PAGE_DOWN")
      , (35, "END")
      , (36, "HOME")
      , (37, "LEFT")
      , (38, "UP")
      , (39, "RIGHT")
      , (40, "DOWN")
      , (46, "DELETE")
      ]

matchingKeyName :: CompiledProgram -> String -> Maybe String
matchingKeyName prog keyName =
  case find matches (map fst (cpHandlers prog)) of
    Just (KeyPress actualName) -> Just actualName
    _                          -> Nothing
  where
    matches (KeyPress actualName) = normalize actualName == normalize keyName
    matches _                    = False
    normalize = map toUpper