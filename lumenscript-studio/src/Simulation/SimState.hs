module Simulation.SimState
  ( SimState(..)
  , initSimState
  , mkSimHostEnv
  , stepEvent
  , runQueued
  ) where

import Data.IORef
import qualified Data.Map as Map

import AST (Ident, EventSpec)
import VM.VM (HostEnv(..), VarEnv, runEvent)
import VM.EventTrigger (CompiledProgram)
import VM.Stack (Value(..))
import Simulation.GridWorld

-- | Everything the simulation needs between events: the world sprites
-- live in, the VM's variable store (threaded across events, same as
-- runEvent expects), and a running log of every LOAD/CALL for a GUI
-- or CLI to display as an event trace.
data SimState = SimState
  { simWorld :: GridWorld
  , simVars  :: VarEnv
  , simLog   :: [String]  -- ^ newest entry first
  } deriving (Show)

initSimState :: Int -> Int -> SimState
initSimState w h = SimState
  { simWorld = initGridWorld w h
  , simVars  = Map.empty
  , simLog   = []
  }

appendLog :: IORef SimState -> String -> IO ()
appendLog ref msg = modifyIORef' ref (\st -> st { simLog = msg : simLog st })

-- | Build a HostEnv wired to a mutable SimState. VM.VM's HostEnv is
-- IO-based by design (see its docs in VM.hs), so an IORef is the
-- natural way to let LOAD/CALL instructions read and mutate the grid
-- world in place while the VM's own core loop stays untouched.
mkSimHostEnv :: IORef SimState -> HostEnv
mkSimHostEnv ref = HostEnv
  { hostLoad = \obj field -> do
      st <- readIORef ref
      case gridLoad obj field (simWorld st) of
        Just v -> do
          appendLog ref ("[sim] " ++ obj ++ "." ++ field ++ " -> " ++ show v)
          pure v
        Nothing -> do
          -- Shouldn't happen if Semantic.ScopeResolution/TypeCheck ran
          -- first, but don't crash the simulation over it.
          appendLog ref ("[sim] unknown property: " ++ obj ++ "." ++ field)
          pure VUnit
  , hostCall = \obj method args -> do
      st <- readIORef ref
      let (world', result) = gridCall obj method args (simWorld st)
      writeIORef ref st { simWorld = world' }
      appendLog ref ("[sim] " ++ obj ++ "." ++ method ++ show args)
      pure result
  }

-- | Step-by-step mode: run exactly one event to completion and stop.
-- This is the primitive a GUI "Step" button (or a single CLI command)
-- would call once per user action.
stepEvent :: IORef SimState -> CompiledProgram -> EventSpec -> IO ()
stepEvent ref prog spec = do
  st <- readIORef ref
  let host = mkSimHostEnv ref
  vars' <- runEvent host prog spec (simVars st)
  modifyIORef' ref (\s -> s { simVars = vars' })

-- | Continuous mode: drain a queue of events back-to-back with no
-- pause for user input between them (e.g. replaying a recorded input
-- log, or auto-advancing a demo). Each event still runs to completion
-- via stepEvent before the next starts, so log ordering stays
-- deterministic regardless of how fast this is called.
runQueued :: IORef SimState -> CompiledProgram -> [EventSpec] -> IO ()
runQueued ref prog = mapM_ (stepEvent ref prog)