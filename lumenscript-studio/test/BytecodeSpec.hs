module BytecodeSpec (spec) where

import Test.Hspec
import AST
import Bytecode.Instruction
import Bytecode.Generator

spec :: Spec
spec = do
  describe "Bytecode Generator" $ do
    it "handles variable declarations" $
      let ast = [Decl "health" (LitInt 100)]
          expected = Right $ CompiledProgram [PUSH_INT 100, STORE "health"] []
      in generateBytecode ast `shouldBe` expected

    it "handles variable assignments" $
      let ast = [Assign "health" (Binary Sub (Var "health") (LitInt 10))]
          expected = Right $ CompiledProgram [LOAD "health", PUSH_INT 10, OP Sub, STORE "health"] []
      in generateBytecode ast `shouldBe` expected

    it "respects arithmetic precedence" $
      let ast = [ExprStmt (Binary Add (Binary Mul (LitInt 2) (LitInt 5)) (LitInt 3))]
          expected = Right $ CompiledProgram [PUSH_INT 2, PUSH_INT 5, OP Mul, PUSH_INT 3, OP Add] []
      in generateBytecode ast `shouldBe` expected

    it "handles unary operations" $
      let ast = [ExprStmt (Unary Not (Var "alive"))]
          expected = Right $ CompiledProgram [LOAD "alive", UOP Not] []
      in generateBytecode ast `shouldBe` expected

    it "handles function calls without arguments" $
      let ast = [ExprStmt (Call (Var "player") [])]
          expected = Right $ CompiledProgram [LOAD "player", CALL "Var \"player\"" 0] []
      in generateBytecode ast `shouldBe` expected

    it "handles function calls with arguments" $
      let ast = [ExprStmt (Call (Var "dialogue") [LitStr "Hi"])]
          expected = Right $ CompiledProgram [PUSH_STR "Hi", LOAD "dialogue", CALL "Var \"dialogue\"" 1] []
      in generateBytecode ast `shouldBe` expected

    it "generates correct labels for if statements" $
      let ast = [If (LitBool True) [ExprStmt (LitInt 1)] Nothing]
          -- This test is tricky without inspecting labels.
          -- We'll check the instruction shapes.
          Right (CompiledProgram main _) = generateBytecode ast
      in main `shouldSatisfy` (\is -> case is of
            [PUSH_BOOL True, JUMP_IF_FALSE "L0", PUSH_INT 1, LABEL "L0"] -> True
            _ -> False)

    it "separates main block and event handlers" $
      let ast = [
            Decl "x" (LitInt 10),
            OnEvent (KeyPress "SPACE") [ExprStmt (Call (Var "player") [])]
            ]
          expected = Right $ CompiledProgram
            { mainBlock = [PUSH_INT 10, STORE "x"]
            , handlers = [("key_press_SPACE", [LOAD "player", CALL "Var \"player\"" 0])]
            }
      in generateBytecode ast `shouldBe` expected