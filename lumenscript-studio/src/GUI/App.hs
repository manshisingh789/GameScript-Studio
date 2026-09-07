module GUI.App (runGui) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.Map as Map
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Compiler (compile, CompilationResult(..))
import Lexer (lexer)
import Parser (parseProgram)
import VM.EventTrigger (CompiledProgram(..))
import Simulation.SimState

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
-- the real pipeline end to end.
--
-- All four viewer panels reflect real data after a compile:
--
--   * Tokens   -- the real output of Lexer.lexer on the editor's text.
--   * AST      -- the real output of Parser.parseProgram on those
--                tokens (shown independently of whether the later
--                semantic/bytecode stages succeed, so you can see the
--                AST even when Compiler.compile reports a semantic
--                error).
--   * Bytecode -- Compiler.compile's real compiled output, rendered
--                one handler and one instruction per line via
--                GUI.BytecodeViewer.formatCompiledProgram (rather than
--                CompiledProgram's derived Show, which would render
--                the whole record -- including every instruction --
--                as a single unreadable line).
--   * Stack Trace -- every event handler registered in the compiled
--                program is actually run once, in order, through
--                Simulation.SimState against a real HostEnv/GridWorld.
--                The panel shows the real final variables and the
--                real LOAD/CALL trace collected along the way. This
--                is a post-run summary, not a live single-instruction
--                stepper -- VM.VM doesn't currently export the
--                per-instruction primitives (`step`, `findLabelIndex`,
--                `handlerEnd`) that a "Step" button would need; those
--                would need to be added to VM.VM's export list first.
--
-- Note: this calls Lexer.lexer and Parser.parseProgram directly, in
-- addition to Compiler.compile (which almost certainly calls the same
-- two functions internally). That's a small amount of duplicated
-- work, accepted here because Compiler.hs doesn't currently expose
-- its intermediate token stream or parsed AST on success -- only the
-- final CompiledProgram or an error. Both functions are pure, so
-- calling them twice is harmless beyond the minor recomputation.
--
-- Persistence: 'savedSourceRef' is created exactly once, right here,
-- before 'startGUI' is called -- not inside 'setup'. Threepenny calls
-- 'setup' fresh for every page load/refresh (each one is a new
-- session), so anything created inside 'setup' resets every time. By
-- creating the IORef outside it and capturing it in the closure passed
-- to 'startGUI', every session shares the same reference, so a refresh
-- reopens with whatever you last typed instead of snapping back to
-- 'demoSource'. This only lasts as long as the server process itself
-- keeps running -- stopping and relaunching `--gui` still starts fresh,
-- since nothing is written to disk.
runGui :: IO ()
runGui = do
  savedSourceRef <- newIORef demoSource
  startGUI defaultConfig (setup savedSourceRef)

setup :: IORef String -> Window -> UI ()
setup savedSourceRef window = do
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

    -- Tokens: always real. Lexing can't fail outright -- bad
    -- characters just turn into TError tokens in the stream -- so
    -- there's no error case to handle here.
    let tokens = lexer source
    updateTokenViewer tokenViewer (map show tokens)

    -- AST: real parse of those same tokens, shown independently of
    -- Compiler.compile's result below. If parsing fails, show the
    -- real parse error instead of a stale/dummy AST.
    case parseProgram tokens of
      Right (program, _leftoverTokens) ->
        updateASTViewer astViewer (map show program)
      Left parseErr ->
        updateASTViewer astViewer ["Parse failed: " ++ show parseErr]

    case compile source of
      CompilationSuccess compiledProgram -> do
        void $ element (epStatus editor) # set UI.text "Compiled successfully."
        updateBytecodeViewer bytecodeViewer (formatCompiledProgram compiledProgram)
        traceLines <- runAllHandlers compiledProgram
        updateStackTraceViewer stackTraceViewer traceLines

      SemanticErrors errs -> do
        void $ element (epStatus editor) # set UI.text
          ("Semantic errors:\n" ++ unlines (map show errs))
        updateStackTraceViewer stackTraceViewer
          ["(compile failed -- nothing to run)"]

      ParseError err -> do
        void $ element (epStatus editor) # set UI.text
          ("Parse error: " ++ show err)
        updateStackTraceViewer stackTraceViewer
          ["(compile failed -- nothing to run)"]

-- | Actually run every event handler registered in the compiled
-- program once, in the order Bytecode.Generator recorded them,
-- against a fresh simulation world -- a real VM run through
-- Simulation.SimState.stepEvent, not fabricated data. Reports which
-- handlers ran, the final variable store, and the full LOAD/CALL
-- trace collected along the way.
runAllHandlers :: CompiledProgram -> UI [String]
runAllHandlers prog = liftIO $ do
  ref <- newIORef (initSimState simWidth simHeight)
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