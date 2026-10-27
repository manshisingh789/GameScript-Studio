module LinkBuildSpec (spec) where

import Control.Monad (forM_)
import qualified Data.IntSet as IS
import Test.Hspec

import Compiler (CompilationResult(..), PipelineStages(..), compileStages)
import VM.EventTrigger (CompiledProgram(..))
import LinkBuild (buildLinkTable)
import SourceLink

tableFor :: String -> Either String LinkTable
tableFor src =
  let st = compileStages src
  in case psParse st of
       Left e            -> Left (show e)
       Right (prog, _)   -> Right (buildLinkTable src (psTokens st) prog (instrsOf (psResult st)))
  where
    instrsOf (CompilationSuccess cp) = Just (cpInstrs cp)
    instrsOf _                       = Nothing

-- (instruction label without its index, exact source text it came from)
codeTexts :: LinkTable -> [(String, String)]
codeTexts lt =
  [ (dropWhile (== ' ') (drop 5 (itLabel itm)), take (e - s) (drop s (ltSource lt)))
  | itm <- ltCode lt, Just (Span s e) <- [itSpan itm] ]

-- (label without indentation, exact source text of the node)
astTexts :: LinkTable -> [(String, String)]
astTexts lt =
  [ (dropWhile (== ' ') (itLabel itm), take (e - s) (drop s (ltSource lt)))
  | itm <- ltAst lt, Just (Span s e) <- [itSpan itm] ]

spec :: Spec
spec = describe "LinkBuild (real Lexer + Parser)" $ do

  it "demo program: Decl, OnEvent and Call get exact source spans" $ do
    let src = unlines [ "let x = 5", "on key_press SPACE: {", "  player.jump()", "}" ]
    case tableFor src of
      Left e   -> expectationFailure e
      Right lt -> do
        astTexts lt `shouldSatisfy` elem ("Decl let x", "let x = 5")
        astTexts lt `shouldSatisfy` elem ("Call", "player.jump()")
        astTexts lt `shouldSatisfy` any (\(l, t) -> take 7 l == "OnEvent" && last t == '}')

  it "arithmetic respects precedence in the spans" $
    case tableFor "let y = a + b * 2\n" of
      Left e   -> expectationFailure e
      Right lt -> do
        astTexts lt `shouldSatisfy` elem ("Binary Add", "a + b * 2")
        astTexts lt `shouldSatisfy` elem ("Binary Mul", "b * 2")

  it "every token span lies inside the source" $
    case tableFor "let x = 5\n" of
      Left e   -> expectationFailure e
      Right lt -> forM_ (ltTokens lt) $ \itm -> case itSpan itm of
        Just (Span a b) -> (a >= 0 && b <= length (ltSource lt)) `shouldBe` True
        Nothing         -> expectationFailure "token without span"

  -- Catches any Position convention that differs from what LinkBuild assumes:
  it "no AST node of any demo script is left without a source span" $
    forM_ [ "demos/dialogue_tree.lum", "demos/enemy_ai.lum", "demos/platformer.lum" ] $ \path -> do
      src <- readFile path
      case tableFor src of
        Left _   -> pure ()                       -- parse failure is DemoSpec's concern
        Right lt -> [ l | itm <- ltAst lt, itSpan itm == Nothing, let l = itLabel itm ] `shouldBe` []

  it "bytecode is aligned with the AST and every instruction has a source span" $ do
    let src = unlines [ "let x = 5", "on key_press SPACE: {", "  player.jump()", "}" ]
    case tableFor src of
      Left e   -> expectationFailure e
      Right lt -> do
        ltNote lt `shouldBe` ""
        ltCode lt `shouldSatisfy` (not . null)
        codeTexts lt `shouldSatisfy` any (\(l, t) -> take 9 l == "STORE_VAR" && t == "let x = 5")
        codeTexts lt `shouldSatisfy` any (\(l, t) -> take 4 l == "CALL" && t == "player.jump()")

  -- Fails if Semantic.analyzeProgram rewrites the AST so that the generated
  -- instructions no longer line up with the parsed AST.
  it "every demo script's bytecode aligns with its AST (ltNote is empty)" $
    forM_ [ "demos/dialogue_tree.lum", "demos/enemy_ai.lum", "demos/platformer.lum" ] $ \path -> do
      src <- readFile path
      case tableFor src of
        Left _   -> pure ()
        Right lt -> ltNote lt `shouldBe` ""

  it "clicking an instruction lights the tokens it came from" $
    case tableFor "let x = 5\n" of
      Left e   -> expectationFailure e
      Right lt ->
        -- instruction 0 is PUSH_INT 5 -> exactly the token "5"
        IS.toList (hlTokens (resolve lt (SelCode 0))) `shouldBe`
          [ i | (i, itm) <- zip [0 ..] (ltTokens lt), itLabel itm == "TInt 5" ]