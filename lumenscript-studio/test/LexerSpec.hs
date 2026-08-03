module LexerSpec (spec) where

import Test.Hspec
import Token
import Lexer

isError :: Token -> Bool
isError (Token TInvalid _) = True
isError (Token (TError _) _) = True
isError _ = False

spec :: Spec
spec = describe "Lexer" $ do

  describe "scanNumber" $ do
    it "scans a simple integer" $ do
      let (tok, rest, pos) = scanNumber "123abc" (Position 1 1)
      tokenType tok `shouldBe` TInt 123
      tokenPosition tok `shouldBe` Position 1 1
      rest `shouldBe` "abc"
      pos `shouldBe` Position 1 4

    it "scans integer at start of string" $ do
      let (tok, rest, pos) = scanNumber "42" (Position 1 1)
      tokenType tok `shouldBe` TInt 42
      rest `shouldBe` ""
      pos `shouldBe` Position 1 3

  describe "lexer" $ do
    it "handles a single newline" $ do
      let tokens = lexer "let\nx"
      map tokenType tokens `shouldBe` [TKwLet, TNewline, TIdent "x", TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 4, Position 2 1, Position 2 2]

    it "collapses multiple newlines" $ do
      let tokens = lexer "let\n\nx"
      map tokenType tokens `shouldBe` [TKwLet, TBlank, TIdent "x", TEOF]

    it "emits a separator after each completed statement" $ do
      let tokens = lexer "let health = 100\nlet score = 0\n"
      map tokenType tokens `shouldBe`
        [ TKwLet, TIdent "health", TAssign, TInt 100, TNewline
        , TKwLet, TIdent "score", TAssign, TInt 0, TNewline, TEOF
        ]

    it "does not emit extra tokens for blank lines" $ do
      let tokens = lexer "let health = 100\n  \t\n\nlet score = 0\n"
      map tokenType tokens `shouldBe`
        [ TKwLet, TIdent "health", TAssign, TInt 100, TBlank, TNewline
        , TKwLet, TIdent "score", TAssign, TInt 0, TNewline, TEOF
        ]

    it "separates tokens with newlines" $ do
      let tokens = lexer "1\n2"
      map tokenType tokens `shouldBe` [TInt 1, TNewline, TInt 2, TEOF]

    it "ignores a comment line" $ do
      let tokens = lexer "# this is a comment"
      map tokenType tokens `shouldBe` [TEOF]

    it "ignores a comment at the end of a line" $ do
      let tokens = lexer "let x = 1 # assignment"
      map tokenType tokens `shouldBe` [TKwLet, TIdent "x", TAssign, TInt 1, TEOF]

    it "preserves the newline after a comment and advances the line number" $ do
      let tokens = lexer "let x = 5\n# hello\nlet y = 10"
      map tokenType tokens `shouldBe`
        [ TKwLet, TIdent "x", TAssign, TInt 5, TBlank
        , TKwLet, TIdent "y", TAssign, TInt 10, TEOF
        ]

    it "reports an invalid character" $ do
      let tokens = lexer "let x = 1 ~ 2"
      let errorToken = head $ filter isError tokens
      tokenType errorToken `shouldBe` TInvalid
      tokenPosition errorToken `shouldBe` Position 1 11

    it "reports an unknown symbol" $ do
      let tokens = lexer "let x = @"
      let errorToken = head $ filter isError tokens
      tokenType errorToken `shouldBe` TInvalid
      tokenPosition errorToken `shouldBe` Position 1 9

    it "handles multiple digits correctly" $ do
      let (tok, rest, pos) = scanNumber "007x" (Position 1 1)
      tokenType tok `shouldBe` TInt 7
      rest `shouldBe` "x"
      pos `shouldBe` Position 1 4

  describe "scanIdentifier" $ do
    it "scans a keyword" $ do
      let (tok, rest, pos) = scanIdentifier "let" (Position 1 1)
      tokenType tok `shouldBe` TKwLet
      rest `shouldBe` ""
      pos `shouldBe` Position 1 4

    it "scans a simple identifier" $ do
      let (tok, rest, pos) = scanIdentifier "player" (Position 1 1)
      tokenType tok `shouldBe` TIdent "player"
      rest `shouldBe` ""
      pos `shouldBe` Position 1 7

    it "scans an identifier with an underscore" $ do
      let (tok, rest, pos) = scanIdentifier "key_press" (Position 1 1)
      tokenType tok `shouldBe` TKwKeyPress
      rest `shouldBe` ""
      pos `shouldBe` Position 1 10

  describe "scanString" $ do
    it "scans a simple string" $ do
      let (tok, rest, pos) = scanString "\"Hello\"" (Position 1 1)
      tokenType tok `shouldBe` TString "Hello"
      rest `shouldBe` ""
      pos `shouldBe` Position 1 8

    it "scans a string with spaces" $ do
      let (tok, rest, pos) = scanString "\"Game Over\"" (Position 1 1)
      tokenType tok `shouldBe` TString "Game Over"
      rest `shouldBe` ""
      pos `shouldBe` Position 1 12

    it "handles an unterminated string" $ do
      let tokens = lexer "\"hello"
      let errorToken = head $ filter isError tokens
      tokenType errorToken `shouldBe` TError "Unterminated string"
      tokenPosition errorToken `shouldBe` Position 1 1

    it "reports an invalid character" $ do
      let tokens = lexer "@"
      let errorToken = head $ filter isError tokens
      tokenType errorToken `shouldBe` TInvalid
      tokenPosition errorToken `shouldBe` Position 1 1

    it "reports another invalid character" $ do
      let tokens = lexer "$"
      let errorToken = head $ filter isError tokens
      tokenType errorToken `shouldBe` TInvalid
      tokenPosition errorToken `shouldBe` Position 1 1
  describe "scanOperator" $ do
    it "scans a two-character operator" $ do
      let (tok, rest, pos) = scanOperator ">= " (Position 1 1)
      tokenType tok `shouldBe` TGe
      rest `shouldBe` " "
      pos `shouldBe` Position 1 3

    it "scans a one-character operator" $ do
      let (tok, rest, pos) = scanOperator "+ " (Position 1 1)
      tokenType tok `shouldBe` TPlus
      rest `shouldBe` " "
      pos `shouldBe` Position 1 2

  describe "Golden Tests" $ do
    it "parses a simple assignment" $ do
      let tokens = lexer "let health = 100"
      map tokenType tokens `shouldBe` [TKwLet, TIdent "health", TAssign, TInt 100, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 5, Position 1 12, Position 1 14, Position 1 17]

    it "parses a subtraction" $ do
      let tokens = lexer "health = health - 10"
      map tokenType tokens `shouldBe` [TIdent "health", TAssign, TIdent "health", TMinus, TInt 10, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 8, Position 1 10, Position 1 17, Position 1 19, Position 1 21]

    it "parses a method call" $ do
      let tokens = lexer "player.jump()"
      map tokenType tokens `shouldBe` [TIdent "player", TDot, TIdent "jump", TLParen, TRParen, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 7, Position 1 8, Position 1 12, Position 1 13, Position 1 14]

    it "parses an event handler" $ do
      let tokens = lexer "on key_press SPACE:"
      map tokenType tokens `shouldBe` [TKwOn, TKwKeyPress, TIdent "SPACE", TColon, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 4, Position 1 14, Position 1 19, Position 1 20]

    it "parses an if statement" $ do
      let tokens = lexer "if health <= 0:"
      map tokenType tokens `shouldBe` [TKwIf, TIdent "health", TLe, TInt 0, TColon, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 4, Position 1 11, Position 1 14, Position 1 15, Position 1 16]

    it "parses a function call with a string literal" $ do
      let tokens = lexer "dialogue.show(\"Game Over\")"
      map tokenType tokens `shouldBe` [TIdent "dialogue", TDot, TIdent "show", TLParen, TString "Game Over", TRParen, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 9, Position 1 10, Position 1 14, Position 1 15, Position 1 26, Position 1 27]

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

  describe "scanOperator" $ do
    it "prefers two-character operators" $ do
      let (tok, rest, pos) = scanOperator "== " (Position 1 1)
      tokenType tok `shouldBe` TEq
      rest `shouldBe` " "
      pos `shouldBe` Position 1 3

  describe "scanSymbol" $ do
    it "scans a left parenthesis" $ do
      let (tok, rest, pos) = scanSymbol "(x" (Position 1 1)
      tokenType tok `shouldBe` TLParen
      rest `shouldBe` "x"
      pos `shouldBe` Position 1 2

    it "scans a comma" $ do
      let (tok, rest, pos) = scanSymbol ", " (Position 1 1)
      tokenType tok `shouldBe` TComma
      rest `shouldBe` " "
      pos `shouldBe` Position 1 2