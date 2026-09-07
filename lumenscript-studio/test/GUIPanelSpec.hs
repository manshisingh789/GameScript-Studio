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
        [ "(no event handlers)"
        , ""
        , "Instructions:"
        , "  PUSH_INT 5"
        , "  STORE_VAR \"x\""
        ]

    it "lists each handler on its own line, mapping event spec to entry label" $ do
      let prog = CompiledProgram
            { cpInstrs   = [LABEL "key_press_SPACE", PUSH_INT 1, POP]
            , cpHandlers = [(KeyPress "SPACE", "key_press_SPACE")]
            }
      formatCompiledProgram prog `shouldBe`
        [ "Handlers:"
        , "  " ++ show (KeyPress "SPACE") ++ " -> key_press_SPACE"
        , ""
        , "Instructions:"
        , "  LABEL \"key_press_SPACE\""
        , "  PUSH_INT 1"
        , "  POP"
        ]

    it "says so plainly when there are no instructions at all" $ do
      let prog = CompiledProgram { cpInstrs = [], cpHandlers = [] }
      formatCompiledProgram prog `shouldBe`
        [ "(no event handlers)"
        , ""
        , "Instructions:"
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