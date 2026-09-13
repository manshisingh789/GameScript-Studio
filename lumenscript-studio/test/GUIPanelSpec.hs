module GUIPanelSpec (spec) where

import Test.Hspec

import AST (EventSpec(..))
import Bytecode.Instruction (Instr(..))
import VM.EventTrigger (CompiledProgram(..))
import GUI.Panel (formatAsLines, placeholderIfEmpty)
import qualified GUI.TokenViewer as TV
import qualified GUI.ASTViewer as AV
import GUI.BytecodeViewer (formatCompiledProgram)
import qualified GUI.BytecodeViewer as BV
import qualified GUI.StackTraceViewer as SV
import VM.Stack (Value(..), emptyStack, push)
import VM.VM (VMState(..))

-- | A tiny helper type with a hand-written, deliberately multi-line
-- Show instance, purely so 'formatAsLines' has something real to
-- split on -- most everyday Show instances (Int, lists, records)
-- render on a single line.
data MultiLine = MultiLine String String

instance Show MultiLine where
  show (MultiLine a b) = "line: " ++ a ++ "\nline: " ++ b

spec :: Spec
spec = do
  describe "formatAsLines" $ do
    it "renders a simple Int as a single line matching its Show output" $
      formatAsLines (42 :: Int) `shouldBe` ["42"]

    it "renders a list as a single line, since its Show has no embedded newlines" $
      formatAsLines [1, 2, 3 :: Int] `shouldBe` ["[1,2,3]"]

    it "splits a multi-line Show instance into one entry per line" $
      formatAsLines (MultiLine "a" "b") `shouldBe` ["line: a", "line: b"]

  describe "placeholderIfEmpty" $ do
    it "returns the placeholder message when given an empty list" $
      placeholderIfEmpty "nothing here" [] `shouldBe` ["nothing here"]

    it "returns the original lines unchanged when non-empty" $
      placeholderIfEmpty "nothing here" ["a", "b"] `shouldBe` ["a", "b"]

  describe "formatCompiledProgram" $ do
    it "lists each instruction on its own line, in order" $ do
      let prog = CompiledProgram
            { cpInstrs   = [PUSH_INT 5, STORE_VAR "x"]
            , cpHandlers = []
            }
      formatCompiledProgram prog `shouldBe`
        [ "MAIN"
        , "----"
        , "0  PUSH_INT 5"
        , "1  STORE_VAR \"x\""
        ]

    it "lists each handler on its own line, mapping event spec to entry label" $ do
      let prog = CompiledProgram
            { cpInstrs   = [LABEL "key_press_SPACE", PUSH_INT 1, POP]
            , cpHandlers = [(KeyPress "SPACE", "key_press_SPACE")]
            }
      formatCompiledProgram prog `shouldBe`
        [ "MAIN"
        , "----"
        , "(no instructions)"
        , ""
        , "HANDLER: SPACE"
        , "--------------"
        , "0  LABEL \"key_press_SPACE\""
        , "1  PUSH_INT 1"
        , "2  POP"
        ]

    it "says so plainly when there are no instructions at all" $ do
      let prog = CompiledProgram { cpInstrs = [], cpHandlers = [] }
      formatCompiledProgram prog `shouldBe`
        [ "MAIN"
        , "----"
        , "(no instructions)"
        ]

  describe "viewer placeholder data" $ do
    it "TokenViewer's dummy lines are non-empty" $
      TV.dummyTokenLines `shouldSatisfy` (not . null)

    it "ASTViewer's dummy lines are non-empty" $
      AV.dummyAstLines `shouldSatisfy` (not . null)

    it "BytecodeViewer's dummy lines are non-empty" $
      BV.dummyBytecodeLines `shouldSatisfy` (not . null)

    it "StackTraceViewer's dummy lines are non-empty" $
      SV.dummyStackTraceLines `shouldSatisfy` (not . null)

  describe "live stack trace formatting" $ do
    it "renders the current IP and operand stack top first" $ do
      let state = VMState
            { vmStack = push (VBool True) (push (VInt 10) emptyStack)
            , vmVars = mempty
            , vmPC = 5
            , vmInstrs = []
            }
      SV.formatLiveStackTrace state `shouldBe`
        [ "IP: 5"
        , ""
        , "Operand Stack"
        , "-------------"
        , "[ True ]"
        , "[ 10 ]"
        ]