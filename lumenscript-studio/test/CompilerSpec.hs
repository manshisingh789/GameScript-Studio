module CompilerSpec (spec) where

-- import AST -- IMPORTANT: check why normal import is not working here
import qualified AST
import Test.Hspec
import Compiler
import Bytecode.Instruction
import Semantic.ErrorLog (SemanticError(..))

spec :: Spec
spec = do
  describe "Compiler Pipeline" $ do
    it "compiles valid source code to bytecode" $
      let source = "let x = 10; if x > 5 { x = x - 1; }"
          expectedProgram = CompiledProgram
            [ PUSH_INT 10
            , STORE_LOCAL 0
            , LOAD_LOCAL 0
            , PUSH_INT 5
            , OP AST.Gt
            , JUMP_IF_FALSE "end_if_0"
            , LOAD_LOCAL 0
            , PUSH_INT 1
            , OP AST.Sub
            , STORE_LOCAL 0
            , LABEL "end_if_0"
            ] []
      in compile source `shouldBe` CompilationSuccess expectedProgram

    it "catches semantic errors before bytecode generation" $
      let source = "let x = 10; y = x;" -- y is not defined
          -- This is a simplified expectation. In a real scenario, you'd check for a specific error.
      in case compile source of
           SemanticErrors (_:_) -> pure () -- Success if we get one or more semantic errors
           other -> expectationFailure $ "Expected semantic errors, but got " ++ show other

    it "reports parsing errors" $
      let source = "var x =;" -- Invalid syntax
      in case compile source of
           ParseError _ -> pure () -- Success if we get a parse error
           other -> expectationFailure $ "Expected a parse error, but got " ++ show other