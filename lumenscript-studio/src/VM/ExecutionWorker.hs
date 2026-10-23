module VM.ExecutionWorker
  ( ExecutionCommand(..)
  , ExecutionWorker
  , createExecutionWorker
  , sendExecutionCommand
  ) where

import Control.Concurrent (Chan, forkIO, newChan, readChan, writeChan)
import Control.Concurrent.MVar
import Control.Concurrent (threadDelay)

import VM.VM
  ( HostEnv
  , VMState(..)
  , stepInstruction
  )
import VM.StepControl
  ( ExecMode(..)
  , StepController(..)
  , VMSnapshot
  , snapshotVM
  )

data ExecutionCommand
  = StepCommand
  | RunCommand
  | PauseCommand
  | ResetCommand
  deriving (Eq, Show)

data ExecutionWorker = ExecutionWorker
  { ewCommands :: Chan ExecutionCommand
  }

createExecutionWorker
  :: HostEnv world
  -> StepController world
  -> (VMSnapshot -> IO ())
  -> IO ExecutionWorker
createExecutionWorker host initialController emitSnapshot = do
  commands <- newChan
  controllerRef <- newMVar initialController

  _ <- forkIO (commandLoop commands controllerRef)
  _ <- forkIO (executionLoop controllerRef)

  pure ExecutionWorker
    { ewCommands = commands
    }
  where
    commandLoop commands controllerRef = do
      command <- readChan commands
      case command of
        StepCommand ->
          modifyMVar_ controllerRef $ \controller ->
            if controllerMode controller == Running
              then pure controller
              else do
                (controller', snapshot) <- executeOne controller
                maybe (pure ()) emitSnapshot snapshot
                pure (controller' { controllerMode = Paused })

        RunCommand ->
          modifyMVar_ controllerRef $ \controller ->
            pure $ startController controller

        PauseCommand ->
          modifyMVar_ controllerRef $ \controller ->
            pure $ controller { controllerMode = Paused }

        ResetCommand ->
          modifyMVar_ controllerRef $ \controller -> do
            let next = controller
                  { controllerVMState = controllerInitialState controller
                  , controllerWorld = controllerInitialWorld controller
                  , controllerMode = ResetMode
                  }
            emitSnapshot (snapshotVM (controllerVMState next))
            pure next

      commandLoop commands controllerRef

    executionLoop controllerRef = do
      threadDelay 1000
      snapshot <- modifyMVar controllerRef $ \controller ->
        if controllerMode controller /= Running
          then pure (controller, Nothing)
          else do
            (next, result) <- executeOne controller
            pure (next, result)
      maybe (pure ()) emitSnapshot snapshot
      executionLoop controllerRef

    startController controller
      | vmPC (controllerVMState controller) >= controllerEndPC controller =
          controller { controllerMode = Paused }
      | otherwise =
          controller { controllerMode = Running }

    executeOne controller
      | vmPC (controllerVMState controller) >= controllerEndPC controller =
          pure (controller { controllerMode = Paused }, Nothing)
      | otherwise = do
          (state', world') <-
            stepInstruction host (controllerWorld controller)
              (controllerVMState controller)
          let next = controller
                { controllerVMState = state'
                , controllerWorld = world'
                }
          pure (next, Just (snapshotVM state'))

sendExecutionCommand :: ExecutionWorker -> ExecutionCommand -> IO ()
sendExecutionCommand worker = writeChan (ewCommands worker)