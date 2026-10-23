module VM.StepControl
  ( ExecMode(..)
  , VMSnapshot(..)
  , StepController(..)
  , snapshotVM
  , initStepController
  , stepOnce
  , startExecution
  , pauseExecution
  , resetExecution
  , runContinuously
  ) where

import Control.Concurrent (threadDelay)
import Data.IORef

import VM.Stack (Stack)
import VM.VM
  ( HostEnv
  , VarEnv
  , VMState(..)
  , initVMState
  , stepInstruction
  )
import Bytecode.Instruction (Instr)

-- | The current execution mode.
data ExecMode
  = Stepping
  | Running
  | Paused
  | ResetMode
  deriving (Eq, Show)

-- | A snapshot of the VM state for GUI display.
data VMSnapshot = VMSnapshot
  { snapshotIP    :: Int
  , snapshotStack :: Stack
  , snapshotVars  :: VarEnv
  } deriving (Show)

-- | Capture the VM state for display.
snapshotVM :: VMState -> VMSnapshot
snapshotVM state = VMSnapshot
  { snapshotIP    = vmPC state
  , snapshotStack = vmStack state
  , snapshotVars  = vmVars state
  }

-- | State needed to control execution.
data StepController world = StepController
  { controllerInitialState :: VMState
  , controllerVMState      :: VMState
  , controllerInitialWorld :: world
  , controllerWorld        :: world
  , controllerMode         :: ExecMode
  , controllerEndPC        :: Int
  }

-- | Initialize a new, paused VM session.
initStepController
  :: [Instr]
  -> VarEnv
  -> Int
  -> world
  -> Int
  -> StepController world
initStepController instrs vars startPC world endPC =
  let initialState = initVMState instrs vars startPC
  in StepController
       { controllerInitialState = initialState
       , controllerVMState      = initialState
       , controllerInitialWorld = world
       , controllerWorld        = world
       , controllerMode         = Paused
       , controllerEndPC        = endPC
       }

-- | Execute one instruction without changing the requested execution mode.
executeOne
  :: HostEnv world
  -> StepController world
  -> IO (StepController world, Maybe VMSnapshot)
executeOne host controller
  | vmPC current >= controllerEndPC controller =
      pure
        (controller { controllerMode = Paused }, Nothing)
  | otherwise = do
      (state', world') <-
        stepInstruction host (controllerWorld controller) current

      let controller' = controller
            { controllerVMState = state'
            , controllerWorld   = world'
            }

      pure (controller', Just (snapshotVM state'))
  where
    current = controllerVMState controller

-- | Execute exactly one instruction, then remain paused.
stepOnce
  :: HostEnv world
  -> StepController world
  -> IO (StepController world, Maybe VMSnapshot)
stepOnce host controller
  | controllerMode controller == Running =
      pure (controller, Nothing)
  | otherwise = do
      (controller', snapshot) <- executeOne host controller

      pure
        ( controller' { controllerMode = Paused }
        , snapshot
        )

-- | Request continuous execution.
startExecution :: StepController world -> StepController world
startExecution controller
  | vmPC (controllerVMState controller) >= controllerEndPC controller =
      controller { controllerMode = Paused }
  | otherwise =
      controller { controllerMode = Running }

-- | Pause without changing the VM state.
pauseExecution :: StepController world -> StepController world
pauseExecution controller =
  controller { controllerMode = Paused }

-- | Restore the initial VM state and world.
resetExecution :: StepController world -> StepController world
resetExecution controller =
  controller
    { controllerVMState = controllerInitialState controller
    , controllerWorld   = controllerInitialWorld controller
    , controllerMode    = ResetMode
    }

-- | Run instructions continuously while the controller remains Running.
-- This is a prototype loop; GUI integration must serialize commands and
-- execution to avoid stale-state races.
runContinuously
  :: HostEnv world
  -> IORef (StepController world)
  -> (VMSnapshot -> IO ())
  -> IO ()
runContinuously host controllerRef emitSnapshot = do
  controller <- readIORef controllerRef

  case controllerMode controller of
    Running -> do
      (controller', snapshot) <- executeOne host controller
      writeIORef controllerRef controller'

      case snapshot of
        Nothing -> pure ()
        Just s  -> emitSnapshot s

      threadDelay 1000

      latest <- readIORef controllerRef

      if controllerMode latest == Running
        then runContinuously host controllerRef emitSnapshot
        else pure ()

    _ -> pure ()