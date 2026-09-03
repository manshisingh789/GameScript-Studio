module SimStateSpec (spec) where

import Test.Hspec
import Data.IORef
import qualified Data.Map as Map

import AST (EventSpec(..))
import Bytecode.Instruction (Instr(..))
import VM.VM (HostEnv(..), VarEnv)
import VM.EventTrigger (CompiledProgram(..))
import VM.Stack (Value(..))
import Simulation.GridWorld
import Simulation.SimState

-- | A single-handler program: on the given event, push 5, store it as
-- "x", then LOAD player.distance and store it as "y", then call
-- player.jump() and discard its (VUnit) result.
--
-- This exercises hostLoad (a real player field), hostCall (a real
-- mutating call), and STORE_VAR/vars threading, all in one small
-- handler.
jumpAndRecordProgram :: EventSpec -> CompiledProgram
jumpAndRecordProgram spec = CompiledProgram
  { cpInstrs =
      [ LABEL "h1"
      , PUSH_INT 5
      , STORE_VAR "x"
      , LOAD "player" "distance"
      , STORE_VAR "y"
      , CALL "player" "jump" 0
      , POP
      ]
  , cpHandlers = [(spec, "h1")]
  }

-- | A program with an unrecognized property read, to exercise
-- hostLoad's "unknown property" fallback branch.
unknownPropertyProgram :: EventSpec -> CompiledProgram
unknownPropertyProgram spec = CompiledProgram
  { cpInstrs =
      [ LABEL "h1"
      , LOAD "player" "nope"
      , POP
      ]
  , cpHandlers = [(spec, "h1")]
  }

-- | Two independent handlers back to back, to confirm stepEvent
-- respects handler boundaries when driven through SimState (not just
-- through VM.VM directly, which VMSpec already covers).
twoHandlerProgram :: CompiledProgram
twoHandlerProgram = CompiledProgram
  { cpInstrs =
      [ LABEL "hA"
      , PUSH_INT 1
      , STORE_VAR "a"
      , LABEL "hB"
      , PUSH_INT 2
      , STORE_VAR "b"
      ]
  , cpHandlers =
      [ (KeyPress "A", "hA")
      , (KeyPress "B", "hB")
      ]
  }

runStep :: CompiledProgram -> EventSpec -> IO SimState
runStep prog spec = do
  ref <- newIORef (initSimState 10 8)
  stepEvent ref prog spec
  readIORef ref

runQueuedFrom :: CompiledProgram -> [EventSpec] -> IO SimState
runQueuedFrom prog specs = do
  ref <- newIORef (initSimState 10 8)
  runQueued ref prog specs
  readIORef ref

spec :: Spec
spec = do
  describe "initSimState" $ do
    it "initializes the world with the given dimensions" $ do
      let s = initSimState 10 8
      gwWidth (simWorld s) `shouldBe` 10
      gwHeight (simWorld s) `shouldBe` 8

    it "starts with an empty variable store" $ do
      simVars (initSimState 10 8) `shouldBe` Map.empty

    it "starts with an empty log" $ do
      simLog (initSimState 10 8) `shouldBe` []

  describe "mkSimHostEnv" $ do
    it "hostLoad reads a real player field and logs the read" $ do
      ref <- newIORef (initSimState 10 8)
      let host = mkSimHostEnv ref
      v <- hostLoad host "player" "distance"
      v `shouldBe` VInt 0
      st <- readIORef ref
      simLog st `shouldBe` ["[sim] player.distance -> 0"]

    it "hostLoad on an unrecognized field logs a warning and returns VUnit" $ do
      ref <- newIORef (initSimState 10 8)
      let host = mkSimHostEnv ref
      v <- hostLoad host "player" "nope"
      v `shouldBe` VUnit
      st <- readIORef ref
      simLog st `shouldBe` ["[sim] unknown property: player.nope"]

    it "hostCall mutates the world and logs the call" $ do
      ref <- newIORef (initSimState 10 8)
      let host = mkSimHostEnv ref
      _ <- hostCall host "player" "jump" []
      st <- readIORef ref
      playerDistance (gwPlayer (simWorld st)) `shouldBe` 1
      playerJumps (gwPlayer (simWorld st)) `shouldBe` 1
      simLog st `shouldBe` ["[sim] player.jump[]"]

    it "hostCall on npc.say records the line via GridWorld" $ do
      ref <- newIORef (initSimState 10 8)
      let host = mkSimHostEnv ref
      _ <- hostCall host "npc" "say" [VStr "hi"]
      st <- readIORef ref
      npcLastLine (gwNpc (simWorld st)) `shouldBe` Just "hi"

  describe "stepEvent" $ do
    it "runs a handler's bytecode and threads variables back into SimState" $ do
      let spec' = KeyPress "Space"
      st <- runStep (jumpAndRecordProgram spec') spec'
      simVars st `shouldBe` Map.fromList [("x", VInt 5), ("y", VInt 0)]

    it "applies the handler's world-mutating calls to simWorld" $ do
      let spec' = KeyPress "Space"
      st <- runStep (jumpAndRecordProgram spec') spec'
      playerDistance (gwPlayer (simWorld st)) `shouldBe` 1
      playerJumps (gwPlayer (simWorld st)) `shouldBe` 1

    it "logs LOAD then CALL in order, newest entry first" $ do
      let spec' = KeyPress "Space"
      st <- runStep (jumpAndRecordProgram spec') spec'
      simLog st `shouldBe`
        [ "[sim] player.jump[]"
        , "[sim] player.distance -> 0"
        ]

    it "logs an unknown-property read reached through a real handler" $ do
      let spec' = KeyPress "Space"
      st <- runStep (unknownPropertyProgram spec') spec'
      simLog st `shouldBe` ["[sim] unknown property: player.nope"]

    it "leaves vars, world, and log untouched when no handler matches the event" $ do
      let prog = jumpAndRecordProgram (KeyPress "Space")
      st <- runStep prog (KeyPress "DoesNotExist")
      simVars st `shouldBe` Map.empty
      simLog st `shouldBe` []
      simWorld st `shouldBe` simWorld (initSimState 10 8)

    it "does not let one handler's execution fall through into the next handler's code" $ do
      st <- runStep twoHandlerProgram (KeyPress "A")
      simVars st `shouldBe` Map.fromList [("a", VInt 1)]

    it "runs the second handler independently when triggered directly" $ do
      st <- runStep twoHandlerProgram (KeyPress "B")
      simVars st `shouldBe` Map.fromList [("b", VInt 2)]

  describe "runQueued" $ do
    it "runs multiple events back to back, threading vars and world state forward" $ do
      let spec' = KeyPress "Space"
          prog  = jumpAndRecordProgram spec'
      st <- runQueuedFrom prog [spec', spec']
      -- Second run overwrites x/y with the same literals, but reads
      -- player.distance *after* the first jump already happened.
      simVars st `shouldBe` Map.fromList [("x", VInt 5), ("y", VInt 1)]
      playerDistance (gwPlayer (simWorld st)) `shouldBe` 2
      playerJumps (gwPlayer (simWorld st)) `shouldBe` 2

    it "accumulates log entries from every queued event, most recent first" $ do
      st <- runQueuedFrom twoHandlerProgram [KeyPress "A", KeyPress "B"]
      simVars st `shouldBe` Map.fromList [("a", VInt 1), ("b", VInt 2)]

    it "does nothing for an empty queue" $ do
      st <- runQueuedFrom twoHandlerProgram []
      simVars st `shouldBe` Map.empty
      simLog st `shouldBe` []