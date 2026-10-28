module ApiCatalogCompilerSpec (spec) where

import Control.Monad (forM_)
import Test.Hspec

import ApiCatalog
import Compiler (CompilationResult(..), PipelineStages(..), compileStages)

-- | A script that uses one catalog member inside a known-good handler.
scriptFor :: ApiMember -> String
scriptFor m = unlines
  [ "on key_press SPACE: {"
  , "  " ++ stmt
  , "}" ]
  where stmt = case amKind m of
          KindProperty -> "let v = " ++ insertText m
          _            -> insertText m

spec :: Spec
spec = describe "ApiCatalog vs the real compiler" $ do

  -- If this fails, the explorer lists something the language does not accept:
  -- HostBindings and the semantic analyzer have drifted apart.
  it "the compiler accepts every member the explorer lists" $
    forM_ allMembers $ \m ->
      case psResult (compileStages (scriptFor m)) of
        CompilationSuccess _ -> pure ()
        other -> expectationFailure
          (qualifiedName m ++ " is listed but the compiler rejects it: " ++ show other)

  it "the compiler rejects a member the explorer does not list" $
    case psResult (compileStages "on key_press SPACE: {\n  player.say(\"x\")\n}\n") of
      SemanticErrors _ -> pure ()
      other            -> expectationFailure ("expected a semantic error, got " ++ show other)