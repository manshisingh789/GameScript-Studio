module BytecodeSpec (spec) where

import Test.Hspec
import Data.List (nub)
import AST
import Bytecode.Instruction
import Bytecode.Generator hiding (handlers)
import VM.EventTrigger (CompiledProgram(..))

import Token

dummyPos :: Token.Position
dummyPos = Token.Position 0 0

spec :: Spec
spec = do
  describe "Bytecode Generator" $ do
    it "handles variable declarations" $
      let ast = [Decl "health" Mutable (LitInt 100) dummyPos]
          expected = Right $ CompiledProgram
            { cpInstrs = [PUSH_INT 100, STORE_VAR "health"]
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    it "handles variable assignments" $
      let ast =
            [ Decl "health" Mutable (LitInt 100) dummyPos
            , Assign (Var "health" dummyPos)
                (Binary Sub (Var "health" dummyPos) (LitInt 10) dummyPos)
                dummyPos
            ]
          expected = Right $ CompiledProgram
            { cpInstrs =
                [ PUSH_INT 100, STORE_VAR "health"
                , LOAD_VAR "health", PUSH_INT 10, BINOP Sub, STORE_VAR "health"
                ]
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    -- NOTE: Generator.hs currently treats `Member _ _ _` assignment as a
    -- total no-op (see `generateStmt` for `Assign`): it discards both the
    -- RHS instructions and the LHS target entirely and returns the state
    -- unchanged. So `player.score = 100` compiles to ZERO instructions.
    -- This is almost certainly unintended, but it's what the code does
    -- today. If you add real member-assignment codegen (e.g. a new
    -- STORE_MEMBER instruction), this test needs to change accordingly.
    it "currently compiles member assignments to no instructions (known gap)" $
      let ast = [Assign (Member (Var "player" dummyPos) "score" dummyPos) (LitInt 100) dummyPos]
          expected = Right $ CompiledProgram
            { cpInstrs = []
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    it "respects arithmetic precedence and pops result" $
      let ast = [ExprStmt (Binary Add (Binary Mul (LitInt 2) (LitInt 5) dummyPos) (LitInt 3) dummyPos) ]
          expected = Right $ CompiledProgram
            { cpInstrs = [PUSH_INT 2, PUSH_INT 5, BINOP Mul, PUSH_INT 3, BINOP Add, POP]
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    it "handles unary operations and pops result" $
      let ast = [ExprStmt (Unary Not (Var "alive" dummyPos) dummyPos) ]
          expected = Right $ CompiledProgram
            { cpInstrs = [LOAD_VAR "alive", UNOP Not, POP]
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    it "handles function calls without arguments and pops result" $
      let ast = [ExprStmt (Call (Var "player" dummyPos) [] dummyPos) ]
          -- Plain (non-member) call: objectName is "", no LOAD is
          -- emitted for the callee itself.
          expected = Right $ CompiledProgram
            { cpInstrs = [CALL "" "player" 0, POP]
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    it "handles function calls with arguments and pops result" $
      let ast = [ExprStmt (Call (Var "takeDamage" dummyPos) [LitInt 10] dummyPos) ]
          expected = Right $ CompiledProgram
            { cpInstrs = [PUSH_INT 10, CALL "" "takeDamage" 1, POP]
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    it "handles function calls with multiple arguments and pops result" $
      let ast = [ExprStmt (Call (Var "setPosition" dummyPos) [LitInt 10, LitInt 20] dummyPos) ]
          expected = Right $ CompiledProgram
            { cpInstrs = [PUSH_INT 10, PUSH_INT 20, CALL "" "setPosition" 2, POP]
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    it "generates correct labels for if statements" $
      let ast = [If (LitBool True) [ExprStmt (LitInt 1)] Nothing dummyPos]
          Right (CompiledProgram { cpInstrs = main }) = generateBytecode ast
      in main `shouldBe`
           [PUSH_BOOL True, JUMP_IF_FALSE "end_if_0", PUSH_INT 1, POP, LABEL "end_if_0"]

    it "handles if statements without an else block" $
      let ast =
            [ Decl "health" Mutable (LitInt 50) dummyPos
            , If (Binary Le (Var "health" dummyPos) (LitInt 0) dummyPos)
                 [ExprStmt (Call (Member (Var "player" dummyPos) "jump" dummyPos) [] dummyPos)]
                 Nothing
                 dummyPos
            ]
          expected = Right $ CompiledProgram
            { cpInstrs =
                [ PUSH_INT 50, STORE_VAR "health"
                , LOAD_VAR "health", PUSH_INT 0, BINOP Le
                , JUMP_IF_FALSE "end_if_0"
                , CALL "player" "jump" 0, POP
                , LABEL "end_if_0"
                ]
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    it "handles if-else statements" $
      let ast =
            [ If (Binary Lt (Member (Var "player" dummyPos) "distance" dummyPos) (LitInt 10) dummyPos)
                 [ExprStmt (Call (Member (Var "enemy" dummyPos) "attack" dummyPos) [] dummyPos)]
                 (Just [ExprStmt (Call (Member (Var "enemy" dummyPos) "patrol" dummyPos) [] dummyPos)]) dummyPos
            ]
          expected = Right $ CompiledProgram
            { cpInstrs =
                [ LOAD "player" "distance", PUSH_INT 10, BINOP Lt
                , JUMP_IF_FALSE "else_0"
                , CALL "enemy" "attack" 0, POP
                , JUMP "end_if_1"
                , LABEL "else_0"
                , CALL "enemy" "patrol" 0, POP
                , LABEL "end_if_1"
                ]
            , cpHandlers = []
            }
      in generateBytecode ast `shouldBe` expected

    it "generates unique labels for nested if statements" $
      let ast =
            [ Decl "a" Mutable (LitInt 10) dummyPos
            , Decl "b" Mutable (LitInt 5) dummyPos
            , Decl "x" Mutable (LitInt 0) dummyPos
            , If (Binary Gt (Var "a" dummyPos) (LitInt 5) dummyPos)
                [If (Binary Lt (Var "b" dummyPos) (LitInt 3) dummyPos)
                  [Assign (Var "x" dummyPos) (LitInt 1) dummyPos]
                  Nothing
                  dummyPos
                ]
                Nothing
                dummyPos
            ]
          Right (CompiledProgram { cpInstrs = main }) = generateBytecode ast
          defined = [l | LABEL l <- main]
          used = [l | JUMP l <- main] ++ [l | JUMP_IF_FALSE l <- main]
          allUniqueLabels = nub (defined ++ used)
      in length defined `shouldBe` length allUniqueLabels

    it "separates main block and event handlers" $
      let ast =
            [ Decl "x" Mutable (LitInt 10) dummyPos
            , OnEvent (KeyPress "SPACE") [ExprStmt (Call (Var "player" dummyPos) [] dummyPos)] dummyPos
            ]
          expected = Right $ CompiledProgram
            { cpInstrs =
                [ PUSH_INT 10, STORE_VAR "x"
                , JUMP "after_event_0"
                , LABEL "key_press_SPACE"
                , CALL "" "player" 0, POP
                , LABEL "after_event_0"
                ]
            , cpHandlers = [(KeyPress "SPACE", "key_press_SPACE")]
            }
      in generateBytecode ast `shouldBe` expected