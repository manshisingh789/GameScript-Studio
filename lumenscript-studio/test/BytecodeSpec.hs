module BytecodeSpec (spec) where

import Test.Hspec
import Data.List (nub)
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
      let ast = [Decl "health" (LitInt 100) dummyPos, Assign (Var "health" dummyPos) (Binary Sub (Var "health" dummyPos) (LitInt 10) dummyPos) dummyPos]
          expected = Right $ CompiledProgram [PUSH_INT 100, STORE_LOCAL 0, LOAD_LOCAL 0, PUSH_INT 10, OP Sub, STORE_LOCAL 0] []
      in generateBytecode ast `shouldBe` expected

    it "handles member assignments" $
      let ast = [Assign (Member (Var "player" dummyPos) "score" dummyPos) (LitInt 100) dummyPos]
          expected = Right $ CompiledProgram [LOAD "player", PUSH_INT 100, STORE_MEMBER "score"] []
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

    it "handles function calls with multiple arguments and pops result" $
      let ast = [ExprStmt (Call (Var "setPosition" dummyPos) [LitInt 10, LitInt 20] dummyPos)]
          expected = Right $ CompiledProgram [PUSH_INT 10, PUSH_INT 20, LOAD "setPosition", CALL "setPosition" 2, POP] []
      in generateBytecode ast `shouldBe` expected

    it "generates correct labels for if statements" $
      let ast = [If (LitBool True) [ExprStmt (LitInt 1)] Nothing dummyPos]
          Right (CompiledProgram main _) = generateBytecode ast
      in main `shouldSatisfy` (\is -> case is of
            [PUSH_BOOL True, JUMP_IF_FALSE "end_if_0", PUSH_INT 1, POP, LABEL "end_if_0"] -> True
            _ -> False)

    it "handles if statements without an else block" $
      let ast = [
            Decl "health" (LitInt 50) dummyPos,
            If (Binary Le (Var "health" dummyPos) (LitInt 0) dummyPos)
               [ExprStmt (Call (Member (Var "player" dummyPos) "jump" dummyPos) [] dummyPos)]
               Nothing
               dummyPos
            ]
          expected = Right $ CompiledProgram [
            PUSH_INT 50, STORE_LOCAL 0,
            LOAD_LOCAL 0, PUSH_INT 0, OP Le,
            JUMP_IF_FALSE "end_if_0",
            LOAD "player", LOAD_MEMBER "jump", CALL "player.jump" 0, POP,
            LABEL "end_if_0"
            ] []
      in generateBytecode ast `shouldBe` expected

    it "handles if-else statements" $
      let ast = [
            If (Binary Lt (Member (Var "player" dummyPos) "distance" dummyPos) (LitInt 10) dummyPos)
               [ExprStmt (Call (Member (Var "enemy" dummyPos) "attack" dummyPos) [] dummyPos)]
               (Just [ExprStmt (Call (Member (Var "enemy" dummyPos) "patrol" dummyPos) [] dummyPos)])
               dummyPos
            ]
          expected = Right $ CompiledProgram [
            LOAD "player", LOAD_MEMBER "distance", PUSH_INT 10, OP Lt,
            JUMP_IF_FALSE "else_0",
            LOAD "enemy", LOAD_MEMBER "attack", CALL "enemy.attack" 0, POP,
            JUMP "end_if_1",
            LABEL "else_0",
            LOAD "enemy", LOAD_MEMBER "patrol", CALL "enemy.patrol" 0, POP,
            LABEL "end_if_1"
            ] []
      in generateBytecode ast `shouldBe` expected

    it "generates unique labels for nested if statements" $
      let ast = [
            Decl "a" (LitInt 10) dummyPos,
            Decl "b" (LitInt 5) dummyPos,
            Decl "x" (LitInt 0) dummyPos,
            If (Binary Gt (Var "a" dummyPos) (LitInt 5) dummyPos)
              [If (Binary Lt (Var "b" dummyPos) (LitInt 3) dummyPos)
                [Assign (Var "x" dummyPos) (LitInt 1) dummyPos]
                Nothing
                dummyPos
              ]
              Nothing
              dummyPos
            ]
          Right (CompiledProgram main _) = generateBytecode ast
          defined = [l | LABEL l <- main]
          used = [l | JUMP l <- main] ++ [l | JUMP_IF_FALSE l <- main]
          allUniqueLabels = nub (defined ++ used)
      in length defined `shouldBe` length allUniqueLabels

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