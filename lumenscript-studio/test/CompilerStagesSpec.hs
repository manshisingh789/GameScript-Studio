module CompilerStagesSpec (spec) where

import Test.Hspec

import Compiler
import qualified Lexer

spec :: Spec
spec = do
  describe "compileStages" $ do
    it "produces the same tokens Lexer.lexer would, for a valid program" $ do
      let src = "let x = 5"
      psTokens (compileStages src) `shouldBe` Lexer.lexer src

    it "produces a successful parse and a CompilationSuccess for valid source" $ do
      let src = "let x = 5"
          stages = compileStages src
      case psParse stages of
        Right _ -> pure ()
        Left err -> expectationFailure ("expected a successful parse, got: " ++ show err)
      case psResult stages of
        CompilationSuccess _ -> pure ()
        other -> expectationFailure ("expected CompilationSuccess, got: " ++ show other)

    it "produces a parse failure and a matching ParseError for malformed source" $ do
      let src = "let x 5" -- missing '='
          stages = compileStages src
      case psParse stages of
        Left _ -> pure ()
        Right _ -> expectationFailure "expected a parse failure"
      case psResult stages of
        ParseError _ -> pure ()
        other -> expectationFailure ("expected ParseError, got: " ++ show other)

    it "still returns the successfully-parsed AST even when semantic analysis fails" $ do
      let src = "let x = y" -- y is undefined
          stages = compileStages src
      case psParse stages of
        Right _ -> pure ()
        Left err -> expectationFailure ("expected a successful parse, got: " ++ show err)
      case psResult stages of
        SemanticErrors _ -> pure ()
        other -> expectationFailure ("expected SemanticErrors, got: " ++ show other)

    it "agrees with compile on every result, since compile is defined in terms of it" $ do
      let sources = ["let x = 5", "let x 5", "let x = y"]
      mapM_ (\src -> psResult (compileStages src) `shouldBe` compile src) sources