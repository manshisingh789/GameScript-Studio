module SemanticSpec (spec) where

import Test.Hspec

import AST
import Token hiding (TString, TInt)
import Semantic.ErrorLog (SemanticError(..), Type(..), runSemanticM, semanticOk, hasFatalErrors)
import Semantic.TypeCheck (analyzeProgram)


-- Test helpers

p :: Position
p = Position 1 1

p2 :: Position
p2 = Position 2 1

p3 :: Position
p3 = Position 3 1


validProgram :: Program
validProgram =
  [ Decl "x" (LitInt 10) p
  , Decl "message" (LitStr "hello") p2
  , Decl "flag" (LitBool True) p3
  ]


-- Specification

spec :: Spec
spec = do

  describe "Semantic analysis" $ do

    -- -----------------------------------------------------------------------
    -- Valid programs
    -- -----------------------------------------------------------------------

    describe "valid programs" $ do

      it "passes a program containing valid declarations" $ do
        let (_, errors) = runSemanticM (analyzeProgram validProgram)
        semanticOk errors `shouldBe` True

      it "allows variables to be used after declaration" $ do
        let program =
              [ Decl "x" (LitInt 10) p
              , ExprStmt (Var "x" p2)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "allows integer arithmetic" $ do
        let program =
              [ Decl "x"
                  (Binary Add
                    (LitInt 10)
                    (LitInt 20)
                    p)
                  p
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "allows boolean conditions" $ do
        let program =
              [ If
                  (LitBool True)
                  []
                  Nothing
                  p
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "allows valid unary integer negation" $ do
        let program =
              [ Decl "x"
                  (Unary Neg (LitInt 10) p)
                  p
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "allows valid boolean negation" $ do
        let program =
              [ Decl "flag"
                  (Unary Not (LitBool True) p)
                  p
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True


    -- -----------------------------------------------------------------------
    -- Scope resolution
    -- -----------------------------------------------------------------------

    describe "scope resolution" $ do

      it "reports an undefined variable" $ do
        let program =
              [ ExprStmt (Var "missing" p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ UndefinedVariable "missing" p
            ]

      it "allows a variable declared before it is used" $ do
        let program =
              [ Decl "x" (LitInt 10) p
              , ExprStmt (Var "x" p2)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors `shouldBe` []

      it "reports use of a variable before declaration" $ do
        let program =
              [ ExprStmt (Var "x" p)
              , Decl "x" (LitInt 10) p2
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ UndefinedVariable "x" p
            ]

      it "reports duplicate declarations in the same scope" $ do
        let program =
              [ Decl "x" (LitInt 10) p
              , Decl "x" (LitInt 20) p2
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ DuplicateDeclaration "x" p2 p
            ]

      it "allows shadowing inside an if block" $ do
        let program =
              [ Decl "x" (LitInt 10) p
              , If
                  (LitBool True)
                  [ Decl "x" (LitInt 20) p2
                  , ExprStmt (Var "x" p3)
                  ]
                  Nothing
                  p
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "does not allow a variable declared inside an if block outside the block" $ do
        let program =
              [ If
                  (LitBool True)
                  [ Decl "inside" (LitInt 10) p
                  ]
                  Nothing
                  p2
              , ExprStmt (Var "inside" p3)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ UndefinedVariable "inside" p3
            ]

      it "does not allow a variable declared inside an event body outside the event" $ do
        let program =
              [ OnEvent
                  (KeyPress "SPACE")
                  [ Decl "inside" (LitInt 10) p ]
                  p2
              , ExprStmt (Var "inside" p3)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ UndefinedVariable "inside" p3
            ]


    -- -----------------------------------------------------------------------
    -- Built-in objects and members
    -- -----------------------------------------------------------------------

    describe "built-in objects and members" $ do

      it "recognizes player as a built-in object" $ do
        let program =
              [ ExprStmt (Var "player" p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "allows a valid player property" $ do
        let program =
              [ ExprStmt
                  (Member
                    (Var "player" p)
                    "distance"
                    p2)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "allows another valid player property" $ do
        let program =
              [ ExprStmt
                  (Member
                    (Var "player" p)
                    "level"
                    p2)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "reports an undefined member on a built-in object" $ do
        let program =
              [ ExprStmt
                  (Member
                    (Var "player" p)
                    "health"
                    p2)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ UndefinedMember "player" "health" p2
            ]


    -- -----------------------------------------------------------------------
    -- Built-in function calls
    -- -----------------------------------------------------------------------

    describe "built-in function calls" $ do

      it "allows player.jump() with the correct number of arguments" $ do
        let program =
              [ ExprStmt
                  (Call
                    (Member
                      (Var "player" p)
                      "jump"
                      p2)
                    []
                    p3)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "reports an arity mismatch for player.jump()" $ do
        let program =
              [ ExprStmt
                  (Call
                    (Member
                      (Var "player" p)
                      "jump"
                      p2)
                    [LitInt 10]
                    p3)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ ArityMismatch "player.jump" 0 1 p3
            ]

      it "allows npc.say() with one string argument" $ do
        let program =
              [ ExprStmt
                  (Call
                    (Member
                      (Var "npc" p)
                      "say"
                      p2)
                    [LitStr "Hello"]
                    p3)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "reports an arity mismatch for npc.say() with no arguments" $ do
        let program =
              [ ExprStmt
                  (Call
                    (Member
                      (Var "npc" p)
                      "say"
                      p2)
                    []
                    p3)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ ArityMismatch "npc.say" 1 0 p3
            ]

      it "reports an argument type mismatch for npc.say()" $ do
        let program =
              [ ExprStmt
                  (Call
                    (Member
                      (Var "npc" p)
                      "say"
                      p2)
                    [LitInt 42]
                    p3)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ TypeMismatch
                TString
                TInt
                "argument to npc.say"
                p3
            ]


    -- -----------------------------------------------------------------------
    -- Type checking
    -- -----------------------------------------------------------------------

    describe "type checking" $ do

      it "reports invalid arithmetic operands" $ do
        let program =
              [ ExprStmt
                  (Binary Add
                    (LitInt 10)
                    (LitStr "hello")
                    p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ InvalidOperandType "+" TString p
            ]

      it "reports invalid operands for unary negation" $ do
        let program =
              [ ExprStmt
                  (Unary Neg
                    (LitStr "hello")
                    p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ InvalidOperandType "-" TString p
            ]

      it "reports invalid operands for boolean negation" $ do
        let program =
              [ ExprStmt
                  (Unary Not
                    (LitInt 10)
                    p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ InvalidOperandType "!" TInt p
            ]

      it "reports an invalid if condition" $ do
        let program =
              [ If
                  (LitInt 10)
                  []
                  Nothing
                  p
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ InvalidCondition TInt p
            ]

      it "allows equality comparison between values of the same type" $ do
        let program =
              [ ExprStmt
                  (Binary Eq
                    (LitInt 10)
                    (LitInt 20)
                    p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "reports a type mismatch for equality comparison of different types" $ do
        let program =
              [ ExprStmt
                  (Binary Eq
                    (LitInt 10)
                    (LitStr "10")
                    p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ TypeMismatch
                TInt
                TString
                "comparison"
                p
            ]

      it "allows relational comparison between integers" $ do
        let program =
              [ ExprStmt
                  (Binary Lt
                    (LitInt 10)
                    (LitInt 20)
                    p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True

      it "reports invalid operands for relational comparison of non-integers" $ do
        let program =
              [ ExprStmt
                  (Binary Lt
                    (LitStr "a")
                    (LitStr "b")
                    p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ InvalidOperandType "<" TString p
            ]

      it "reports assignment type mismatch" $ do
        let program =
              [ Decl "x" (LitInt 10) p
              , Assign "x" (LitStr "hello") p2
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        errors
          `shouldBe`
            [ AssignmentTypeMismatch
                "x"
                TInt
                TString
                p2
            ]

      it "allows assignment when the type matches" $ do
        let program =
              [ Decl "x" (LitInt 10) p
              , Assign "x" (LitInt 20) p2
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True


    -- -----------------------------------------------------------------------
    -- Error aggregation
    -- -----------------------------------------------------------------------

    describe "error aggregation" $ do

      it "reports multiple independent semantic errors in one analysis" $ do
        let program =
              [ ExprStmt (Var "missing" p)

              , ExprStmt
                  (Binary Add
                    (LitInt 10)
                    (LitStr "wrong")
                    p2)

              , If
                  (LitInt 42)
                  []
                  Nothing
                  p3
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        hasFatalErrors errors `shouldBe` True
        errors
          `shouldBe`
            [ UndefinedVariable "missing" p
            , InvalidOperandType "+" TString p2
            , InvalidCondition TInt p3
            ]

      it "does not stop after an undefined variable" $ do
        let program =
              [ ExprStmt (Var "missing" p)
              , ExprStmt
                  (Binary Add
                    (LitInt 1)
                    (LitStr "bad")
                    p2)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        length errors `shouldBe` 2

      it "marks semantic analysis as failed whenever any error exists" $ do
        let program =
              [ ExprStmt (Var "missing" p)
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        hasFatalErrors errors `shouldBe` True

      it "marks semantic analysis as passed when no errors exist" $ do
        let program =
              [ Decl "x" (LitInt 10) p
              , Assign "x" (LitInt 20) p2
              ]
            (_, errors) = runSemanticM (analyzeProgram program)
        semanticOk errors `shouldBe` True