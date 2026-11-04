module CompilationStatsSpec (spec) where

import Test.Hspec

import Compiler
import Statistics.CompilationStats

spec :: Spec
spec = describe "compilation statistics availability" $ do
  it "provides all metrics for a valid program" $ do
    let source = "let x = 5"
        stages = compileStages source

    countLogicalLines source `shouldBe` 1
    countTokens (psTokens stages) `shouldSatisfy` (> 0)

    case psParse stages of
      Right (program, _) ->
        countASTNodes program `shouldSatisfy` (> 0)
      Left err ->
        expectationFailure ("expected an AST, got: " ++ show err)

    case psResult stages of
      CompilationSuccess program ->
        countBytecodeInstructions program `shouldSatisfy` (> 0)
      other ->
        expectationFailure ("expected bytecode, got: " ++ show other)

  it "keeps line and token metrics but has no AST or bytecode for a syntax error" $ do
    let source = "let x 5"
        stages = compileStages source

    countLogicalLines source `shouldBe` 1
    countTokens (psTokens stages) `shouldSatisfy` (> 0)
    psParse stages `shouldSatisfy` isParseFailure
    psResult stages `shouldSatisfy` isParseError

    case psParse stages of
      Left _ -> pure ()
      Right (program, _) ->
        expectationFailure ("unexpected AST: " ++ show program)

    case psResult stages of
      ParseError _ -> pure ()
      other -> expectationFailure ("unexpected bytecode result: " ++ show other)

  it "keeps line, token, and AST metrics but has no bytecode for a semantic error" $ do
    let source = "let x = y"
        stages = compileStages source

    countLogicalLines source `shouldBe` 1
    countTokens (psTokens stages) `shouldSatisfy` (> 0)

    case psParse stages of
      Right (program, _) ->
        countASTNodes program `shouldSatisfy` (> 0)
      Left err ->
        expectationFailure ("expected an AST, got: " ++ show err)

    case psResult stages of
      SemanticErrors errors -> errors `shouldSatisfy` (not . null)
      other -> expectationFailure ("expected semantic errors, got: " ++ show other)

    case psResult stages of
      CompilationSuccess program ->
        expectationFailure ("unexpected bytecode: " ++ show program)
      SemanticErrors _ -> pure ()
      ParseError message ->
        expectationFailure ("unexpected parse error: " ++ message)

isParseFailure :: Either parseError parsed -> Bool
isParseFailure result =
  case result of
    Left _ -> True
    Right _ -> False

isParseError :: CompilationResult -> Bool
isParseError result =
  case result of
    ParseError _ -> True
    _ -> False
