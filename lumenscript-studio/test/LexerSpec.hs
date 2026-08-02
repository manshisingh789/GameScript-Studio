module LexerSpec (spec) where

import Test.Hspec
import Token
import Lexer

spec :: Spec
spec = describe "Lexer" $ do

  describe "scanNumber" $ do
    it "scans a simple integer" $ do
      let (tok, rest, pos) = scanNumber "123abc" (Position 1 1)
      tok `shouldBe` TInt 123
      rest `shouldBe` "abc"
      pos `shouldBe` Position 1 4   -- column advanced by 3 digits

    it "scans integer at start of string" $ do
      let (tok, rest, pos) = scanNumber "42" (Position 1 1)
      tok `shouldBe` TInt 42
      rest `shouldBe` ""
      pos `shouldBe` Position 1 3   -- column advanced by 2 digits

    it "handles multiple digits correctly" $ do
      let (tok, rest, pos) = scanNumber "007x" (Position 1 1)
      tok `shouldBe` TInt 7
      rest `shouldBe` "x"
      pos `shouldBe` Position 1 4   -- column advanced by 3 digits
