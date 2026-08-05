module Main where

import Test.Hspec
import qualified LexerSpec
import qualified ParserSpec
import qualified SemanticSpec
import qualified BytecodeSpec
import qualified VMSpec
import qualified DemoSpec

main :: IO ()
main = hspec $ do
  LexerSpec.spec
  DemoSpec.spec
  
  ParserSpec.spec
  SemanticSpec.spec
  BytecodeSpec.spec
  VMSpec.spec