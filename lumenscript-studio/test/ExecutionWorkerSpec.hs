module ExecutionWorkerSpec (spec) where

import Control.Concurrent (threadDelay)
import Data.IORef
import qualified Data.Map as Map
import Test.Hspec

import Bytecode.Instruction (Instr(..))
import VM.ExecutionWorker
import VM.StepControl
import VM.VM (defaultHostEnv)

spec :: Spec
spec =
  describe "execution worker controls" $ do
    it "steps once, runs continuously, pauses, and resets" $ do
      snapshotsRef <- newIORef []
      let instrs =
            [ PUSH_INT 1
            , PUSH_INT 2
            , PUSH_INT 3
            , PUSH_INT 4
            ]
          controller = initStepController instrs Map.empty 0 () (length instrs)
          record snapshot =
            modifyIORef' snapshotsRef (++ [snapshotIP snapshot])

      worker <- createExecutionWorker defaultHostEnv controller record

      threadDelay 10000
      readIORef snapshotsRef `shouldReturn` []

      sendExecutionCommand worker StepCommand
      waitForSnapshotCount snapshotsRef 1
      readIORef snapshotsRef `shouldReturn` [1]

      sendExecutionCommand worker StepCommand
      waitForSnapshotCount snapshotsRef 2
      readIORef snapshotsRef `shouldReturn` [1, 2]

      sendExecutionCommand worker RunCommand
      waitForSnapshotCount snapshotsRef 4
      readIORef snapshotsRef `shouldReturn` [1, 2, 3, 4]

      sendExecutionCommand worker PauseCommand
      threadDelay 10000
      readIORef snapshotsRef `shouldReturn` [1, 2, 3, 4]

      sendExecutionCommand worker ResetCommand
      waitForSnapshotCount snapshotsRef 5
      readIORef snapshotsRef `shouldReturn` [1, 2, 3, 4, 0]

waitForSnapshotCount :: IORef [Int] -> Int -> IO ()
waitForSnapshotCount ref expected = go (100 :: Int)
  where
    go 0 = expectationFailure
      ("timed out waiting for " ++ show expected ++ " VM snapshots")
    go remaining = do
      count <- length <$> readIORef ref
      if count >= expected
        then pure ()
        else threadDelay 1000 >> go (remaining - 1)
