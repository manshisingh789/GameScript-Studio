module CompilerSpec (spec) where

import qualified AST
import Test.Hspec
import Compiler
import Bytecode.Instruction
import VM.EventTrigger (CompiledProgram(..))
import Semantic.ErrorLog (SemanticError(..))

spec :: Spec
spec = do
  describe "Compiler Pipeline" $ do
    it "compiles valid source code to bytecode" $
      let source = "let x = 10; if x > 5: { x = x - 1; }"
      in case compile source of
           CompilationSuccess prog -> do
             -- Structural checks only: this pipeline goes through the real
             -- Lexer/Parser, so the exact instruction sequence depends on
             -- details of those modules I haven't verified by hand (unlike
             -- BytecodeSpec.hs, which calls generateBytecode directly on a
             -- hand-built AST). Run this once, inspect `cpInstrs prog`, and
             -- if it looks right, replace this block with an exact
             -- `prog \`shouldBe\` CompiledProgram { cpInstrs = [...], cpHandlers = [] }`
             -- assertion for a stronger regression test.
             cpInstrs prog `shouldSatisfy` (not . null)
             cpInstrs prog `shouldSatisfy` any isStoreVarX
             cpInstrs prog `shouldSatisfy` any isJumpIfFalse
             cpHandlers prog `shouldBe` []
           other ->
             expectationFailure $ "Expected CompilationSuccess, but got " ++ show other

    it "catches semantic errors before bytecode generation" $
      let source = "let x = 10; y = x;" -- y is not defined
      in case compile source of
           SemanticErrors (_:_) -> pure () -- Success if we get one or more semantic errors
           other -> expectationFailure $ "Expected semantic errors, but got " ++ show other

    it "reports parsing errors" $
      let source = "var x =;" -- Invalid syntax
      in case compile source of
           ParseError _ -> pure () -- Success if we get a parse error
           other -> expectationFailure $ "Expected a parse error, but got " ++ show other

    it "compiles persistent Space and Shift handlers with global calls" $
      let source = unlines
            [ "let jumps = 2"
            , "let canDash = true"
            , "on key_press SPACE: {"
            , "  if jumps > 0: {"
            , "    jump()"
            , "    jumps = jumps - 1"
            , "  } else: {"
            , "    print(\"No jumps remaining.\")"
            , "  }"
            , "}"
            , "on key_press SHIFT: {"
            , "  if canDash: {"
            , "    dash()"
            , "    canDash = false"
            , "  } else: {"
            , "    print(\"Dash unavailable.\")"
            , "  }"
            , "}"
            ]
      in case compile source of
           CompilationSuccess prog -> do
             cpHandlers prog `shouldSatisfy` (not . null)
             cpInstrs prog `shouldSatisfy` any isCall
           other -> expectationFailure $ "Expected CompilationSuccess, but got " ++ show other

isStoreVarX :: Instr -> Bool
isStoreVarX (STORE_VAR "x") = True
isStoreVarX _ = False

isJumpIfFalse :: Instr -> Bool
isJumpIfFalse (JUMP_IF_FALSE _) = True
isJumpIfFalse _ = False

isCall :: Instr -> Bool
isCall (CALL _ _ _) = True
isCall _ = False