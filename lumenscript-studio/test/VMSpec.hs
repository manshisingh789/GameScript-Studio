module VMSpec (spec) where

import Test.Hspec
import qualified Data.Map as Map
import Data.IORef
import Control.Exception (evaluate)

import AST (EventSpec(..))
import Bytecode.Instruction (Instr(..))
import VM.Stack (Value(..), emptyStack)
import VM.VM (defaultHostEnv, HostEnv(..), VMState(..), runEvent, fetchInstr)
import VM.EventTrigger (CompiledProgram(..))

-- Two distinct events used across the tests below.
evtFoo, evtBar :: EventSpec
evtFoo = KeyPress "foo"
evtBar = KeyPress "bar"

mkState :: [Instr] -> Int -> VMState
mkState instrs pc = VMState
  { vmStack  = emptyStack
  , vmVars   = Map.empty
  , vmPC     = pc
  , vmInstrs = instrs
  }

spec :: Spec
spec = do

  describe "JUMP_IF_FALSE / JUMP / LABEL resolution" $ do

    it "takes the then-branch when the condition is True" $ do
      -- LABEL onFoo; PUSH_BOOL True; JUMP_IF_FALSE skip;
      -- PUSH_INT 1; STORE_VAR x; JUMP end;
      -- LABEL skip; PUSH_INT 2; STORE_VAR x;
      -- LABEL end
      let instrs =
            [ LABEL "onFoo"
            , PUSH_BOOL True
            , JUMP_IF_FALSE "skip"
            , PUSH_INT 1
            , STORE_VAR "x"
            , JUMP "end"
            , LABEL "skip"
            , PUSH_INT 2
            , STORE_VAR "x"
            , LABEL "end"
            ]
          prog = CompiledProgram
            { cpInstrs   = instrs
            , cpHandlers = [ (evtFoo, "onFoo") ]
            }
      vars <- runEvent defaultHostEnv prog evtFoo Map.empty
      Map.lookup "x" vars `shouldBe` Just (VInt 1)

    it "takes the else-branch when the condition is False" $ do
      let instrs =
            [ LABEL "onFoo"
            , PUSH_BOOL False
            , JUMP_IF_FALSE "skip"
            , PUSH_INT 1
            , STORE_VAR "x"
            , JUMP "end"
            , LABEL "skip"
            , PUSH_INT 2
            , STORE_VAR "x"
            , LABEL "end"
            ]
          prog = CompiledProgram
            { cpInstrs   = instrs
            , cpHandlers = [ (evtFoo, "onFoo") ]
            }
      vars <- runEvent defaultHostEnv prog evtFoo Map.empty
      Map.lookup "x" vars `shouldBe` Just (VInt 2)

    it "does not fall through past its own handler into the next handler's code" $ do
      -- Two handlers back to back; onFoo's boundary should stop execution
      -- before onBar's PUSH_INT 99 ever runs.
      let instrs =
            [ LABEL "onFoo"
            , PUSH_INT 1
            , STORE_VAR "x"
            , LABEL "onBar"
            , PUSH_INT 99
            , STORE_VAR "x"
            ]
          prog = CompiledProgram
            { cpInstrs   = instrs
            , cpHandlers = [ (evtFoo, "onFoo"), (evtBar, "onBar") ]
            }
      vars <- runEvent defaultHostEnv prog evtFoo Map.empty
      Map.lookup "x" vars `shouldBe` Just (VInt 1)

    it "runs the second handler independently when triggered directly" $ do
      let instrs =
            [ LABEL "onFoo"
            , PUSH_INT 1
            , STORE_VAR "x"
            , LABEL "onBar"
            , PUSH_INT 99
            , STORE_VAR "x"
            ]
          prog = CompiledProgram
            { cpInstrs   = instrs
            , cpHandlers = [ (evtFoo, "onFoo"), (evtBar, "onBar") ]
            }
      vars <- runEvent defaultHostEnv prog evtBar Map.empty
      Map.lookup "x" vars `shouldBe` Just (VInt 99)

  describe "CALL" $ do

    it "pops argc values off the stack in original left-to-right call order" $ do
      capturedArgs <- newIORef []
      let host = defaultHostEnv
            { hostCall = \_obj _method args -> do
                writeIORef capturedArgs args
                pure (VInt 42)
            }
          -- push a1, a2, a3 then CALL obj method 3
          instrs =
            [ LABEL "onFoo"
            , PUSH_INT 1
            , PUSH_INT 2
            , PUSH_INT 3
            , CALL "npc" "say" 3
            , STORE_VAR "result"
            ]
          prog = CompiledProgram
            { cpInstrs   = instrs
            , cpHandlers = [ (evtFoo, "onFoo") ]
            }
      vars <- runEvent host prog evtFoo Map.empty
      Map.lookup "result" vars `shouldBe` Just (VInt 42)
      args <- readIORef capturedArgs
      args `shouldBe` [VInt 1, VInt 2, VInt 3]

    it "passes the object and method names through unchanged" $ do
      capturedCall <- newIORef ("", "")
      let host = defaultHostEnv
            { hostCall = \obj method _args -> do
                writeIORef capturedCall (obj, method)
                pure VUnit
            }
          instrs =
            [ LABEL "onFoo"
            , CALL "npc" "say" 0
            , POP
            ]
          prog = CompiledProgram
            { cpInstrs   = instrs
            , cpHandlers = [ (evtFoo, "onFoo") ]
            }
      _ <- runEvent host prog evtFoo Map.empty
      call <- readIORef capturedCall
      call `shouldBe` ("npc", "say")

  describe "runtime error handling" $ do

    it "errors on stack underflow (POP on empty stack)" $ do
      let instrs = [ LABEL "onFoo", POP ]
          prog = CompiledProgram
            { cpInstrs   = instrs
            , cpHandlers = [ (evtFoo, "onFoo") ]
            }
      runEvent defaultHostEnv prog evtFoo Map.empty
        `shouldThrow` errorCall "[vm] stack underflow: pop on empty stack"

    it "errors with a descriptive message when the PC is out of bounds" $ do
      let instrs = [ LABEL "onFoo", PUSH_INT 1 ]
          badState = mkState instrs 99  -- way past the end of instrs
      evaluate (fetchInstr badState)
        `shouldThrow` errorCall
          "[vm] internal error: program counter out of bounds: 99 (instruction stream has 2 instructions)"

    it "errors with a descriptive message on a negative PC" $ do
      let instrs = [ LABEL "onFoo", PUSH_INT 1 ]
          badState = mkState instrs (-1)
      evaluate (fetchInstr badState)
        `shouldThrow` errorCall
          "[vm] internal error: program counter out of bounds: -1 (instruction stream has 2 instructions)"

    it "errors when jumping to an undefined label" $ do
      let instrs = [ LABEL "onFoo", JUMP "nowhere" ]
          prog = CompiledProgram
            { cpInstrs   = instrs
            , cpHandlers = [ (evtFoo, "onFoo") ]
            }
      runEvent defaultHostEnv prog evtFoo Map.empty
        `shouldThrow` errorCall "[vm] jump to undefined label: nowhere"

    it "returns the unchanged vars and logs when no handler is registered for the event" $ do
      let instrs = [ LABEL "onFoo", PUSH_INT 1, STORE_VAR "x" ]
          prog = CompiledProgram
            { cpInstrs   = instrs
            , cpHandlers = [ (evtFoo, "onFoo") ]  -- evtBar is not registered
            }
          initial = Map.fromList [("x", VInt 0)]
      vars <- runEvent defaultHostEnv prog evtBar initial
      vars `shouldBe` initial