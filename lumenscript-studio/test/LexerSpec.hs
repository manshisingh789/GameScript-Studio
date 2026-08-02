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
      pos `shouldBe` Position 1 3

  describe "lexer" $ do
    it "handles a single newline" $ do
      let tokens = lexer "let\nx"
      tokens `shouldBe` [TKwLet, TNewline, TIdent "x", TEOF]

    it "collapses multiple newlines" $ do
      let tokens = lexer "let\n\nx"
      tokens `shouldBe` [TKwLet, TNewline, TIdent "x", TEOF]

    it "separates tokens with newlines" $ do
      let tokens = lexer "1\n2"
      tokens `shouldBe` [TInt 1, TNewline, TInt 2, TEOF]

    it "ignores a comment line" $ do
      let tokens = lexer "# this is a comment"
      tokens `shouldBe` [TEOF]

    it "ignores a comment at the end of a line" $ do
      let tokens = lexer "let x = 1 # assignment"
      tokens `shouldBe` [TKwLet, TIdent "x", TAssign, TInt 1, TEOF]

    it "ignores an unknown character" $ do
      let tokens = lexer "let ~ x"
      tokens `shouldBe` [TKwLet, TIdent "x", TEOF]

    it "handles multiple digits correctly" $ do
      let (tok, rest, pos) = scanNumber "007x" (Position 1 1)
      tok `shouldBe` TInt 7
      rest `shouldBe` "x"
      pos `shouldBe` Position 1 4   -- column advanced by 3 digits


  describe "scanIdentifier" $ do
    it "scans a keyword" $ do
      let (tok, rest, pos) = scanIdentifier "let" (Position 1 1)
      tok `shouldBe` TKwLet
      rest `shouldBe` ""
      pos `shouldBe` Position 1 4

    it "scans a simple identifier" $ do
      let (tok, rest, pos) = scanIdentifier "player" (Position 1 1)
      tok `shouldBe` TIdent "player"
      rest `shouldBe` ""
      pos `shouldBe` Position 1 7

    it "scans an identifier with an underscore" $ do
      let (tok, rest, pos) = scanIdentifier "key_press" (Position 1 1)
      tok `shouldBe` TKwKeyPress
      rest `shouldBe` ""
      pos `shouldBe` Position 1 10


  describe "scanString" $ do
    it "scans a simple string" $ do
      let (tok, rest, pos) = scanString "\"Hello\"" (Position 1 1)
      tok `shouldBe` TString "Hello"
      rest `shouldBe` ""
      pos `shouldBe` Position 1 8

    it "scans a string with spaces" $ do
      let (tok, rest, pos) = scanString "\"Game Over\"" (Position 1 1)
      tok `shouldBe` TString "Game Over"
      rest `shouldBe` ""
      pos `shouldBe` Position 1 12

    it "handles an unterminated string" $ do
      let (tok, _, _) = scanString "\"unterminated" (Position 1 1)
      tok `shouldBe` TError "Unterminated string"


  describe "scanOperator" $ do
    it "scans a two-character operator" $ do
      let (tok, rest, pos) = scanOperator ">= " (Position 1 1)
      tok `shouldBe` TGe
      rest `shouldBe` " "
      pos `shouldBe` Position 1 3

    it "scans a one-character operator" $ do
      let (tok, rest, pos) = scanOperator "+ " (Position 1 1)
      tok `shouldBe` TPlus
      rest `shouldBe` " "
      pos `shouldBe` Position 1 2


  describe "skipWhitespace" $ do
    it "skips spaces and tabs" $ do
      let (rest, pos) = skipWhitespace "  \t  abc" (Position 1 1)
      rest `shouldBe` "abc"
      pos `shouldBe` Position 1 6

    it "stops at a newline" $ do
      let (rest, pos) = skipWhitespace "  \n" (Position 1 1)
      rest `shouldBe` "\n"
      pos `shouldBe` Position 1 3

    it "stops at other characters" $ do
      let (rest, pos) = skipWhitespace "  x" (Position 1 1)
      rest `shouldBe` "x"
      pos `shouldBe` Position 1 3

    it "prefers two-character operators" $ do
      let (tok, rest, pos) = scanOperator "== " (Position 1 1)
      tok `shouldBe` TEq
      rest `shouldBe` " "
      pos `shouldBe` Position 1 3


  describe "scanSymbol" $ do
    it "scans a left parenthesis" $ do
      let (tok, rest, pos) = scanSymbol "(x" (Position 1 1)
      tok `shouldBe` TLParen
      rest `shouldBe` "x"
      pos `shouldBe` Position 1 2

    it "scans a comma" $ do
      let (tok, rest, pos) = scanSymbol ", " (Position 1 1)
      tok `shouldBe` TComma
      rest `shouldBe` " "
      pos `shouldBe` Position 1 2