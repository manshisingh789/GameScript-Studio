module BytecodeSpec (spec) where

import Test.Hspec
import AST
import Bytecode.Instruction
import Bytecode.Generator hiding (handlers)

import Token

dummyPos :: Token.Position
dummyPos = Token.Position 0 0

spec :: Spec
spec = do
  describe "Bytecode Generator" $ do
    it "handles variable declarations" $
      let ast = [Decl "health" (LitInt 100) dummyPos]
          expected = Right $ CompiledProgram [PUSH_INT 100, STORE_LOCAL 0] []
      in generateBytecode ast `shouldBe` expected

    it "handles variable assignments" $
      let ast = [Decl "health" (LitInt 100) dummyPos, Assign "health" (Binary Sub (Var "health" dummyPos) (LitInt 10) dummyPos) dummyPos]
          expected = Right $ CompiledProgram [PUSH_INT 100, STORE_LOCAL 0, LOAD_LOCAL 0, PUSH_INT 10, OP Sub, STORE_LOCAL 0] []
      in generateBytecode ast `shouldBe` expected

    it "respects arithmetic precedence and pops result" $
      let ast = [ExprStmt (Binary Add (Binary Mul (LitInt 2) (LitInt 5) dummyPos) (LitInt 3) dummyPos)]
          expected = Right $ CompiledProgram [PUSH_INT 2, PUSH_INT 5, OP Mul, PUSH_INT 3, OP Add, POP] []
      in generateBytecode ast `shouldBe` expected

    it "handles unary operations and pops result" $
      let ast = [ExprStmt (Unary Not (Var "alive" dummyPos) dummyPos)]
          expected = Right $ CompiledProgram [LOAD "alive", UOP Not, POP] []
      in generateBytecode ast `shouldBe` expected

    it "handles function calls without arguments and pops result" $
      let ast = [ExprStmt (Call (Var "player" dummyPos) [] dummyPos)]
          expected = Right $ CompiledProgram [LOAD "player", CALL "player" 0, POP] []
      in generateBytecode ast `shouldBe` expected

    it "handles function calls with arguments and pops result" $
      let ast = [ExprStmt (Call (Var "dialogue" dummyPos) [LitStr "Hi"] dummyPos)]
          expected = Right $ CompiledProgram [PUSH_STR "Hi", LOAD "dialogue", CALL "dialogue" 1, POP] []
      in generateBytecode ast `shouldBe` expected

    it "generates correct labels for if statements" $
      let ast = [If (LitBool True) [ExprStmt (LitInt 1)] Nothing dummyPos]
          Right (CompiledProgram main _) = generateBytecode ast
      in main `shouldSatisfy` (\is -> case is of
            [PUSH_BOOL True, JUMP_IF_FALSE "end_if_0", PUSH_INT 1, POP, LABEL "end_if_0"] -> True
            _ -> False)

    it "separates main block and event handlers" $
      let ast = [
            Decl "x" (LitInt 10) dummyPos,
            OnEvent (KeyPress "SPACE") [ExprStmt (Call (Var "player" dummyPos) [] dummyPos)] dummyPos
            ]
          expected = Right $ CompiledProgram
            { mainBlock = [PUSH_INT 10, STORE_LOCAL 0]
            , handlers = [("key_press_SPACE", [LOAD "player", CALL "player" 0, POP])]
            }
      in generateBytecode ast `shouldBe` expected