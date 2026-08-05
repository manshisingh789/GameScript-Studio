module LexerSpec (spec) where

import Test.Hspec
import Token
import Lexer

isError :: Token -> Bool
isError (Token (TError _) _) = True
isError _ = False

spec :: Spec
spec = describe "Lexer" $ do

  describe "Scanning Functions" $ do
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

      it "handles multiple digits correctly" $ do
        let (tok, rest, pos) = scanNumber "007x" (Position 1 1)
        tokenType tok `shouldBe` TInt 7
        rest `shouldBe` "x"
        pos `shouldBe` Position 1 4

      it "handles negative numbers" $ do
        let (tok, rest, pos) = scanNumber "-10" (Position 1 1)
        tokenType tok `shouldBe` TInt (-10)
        rest `shouldBe` ""
        pos `shouldBe` Position 1 4

      it "handles floating point syntax" $ do
        let (tok, rest, pos) = scanNumber "3.14" (Position 1 1)
        tokenType tok `shouldBe` TInt 3
        rest `shouldBe` ".14"
        pos `shouldBe` Position 1 2

      it "handles numbers with underscores" $ do
        let (tok, rest, pos) = scanNumber "1_000_000" (Position 1 1)
        tokenType tok `shouldBe` TInt 1000000
        rest `shouldBe` ""
        pos `shouldBe` Position 1 10

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

      it "scans an identifier starting with an underscore" $ do
        let (tok, rest, pos) = scanIdentifier "_myVar" (Position 1 1)
        tokenType tok `shouldBe` TIdent "_myVar"
        rest `shouldBe` ""
        pos `shouldBe` Position 1 7

      it "scans an identifier containing only an underscore" $ do
        let (tok, rest, pos) = scanIdentifier "_" (Position 1 1)
        tokenType tok `shouldBe` TIdent "_"
        rest `shouldBe` ""
        pos `shouldBe` Position 1 2

      it "scans an identifier with numbers" $ do
        let (tok, rest, pos) = scanIdentifier "player1" (Position 1 1)
        tokenType tok `shouldBe` TIdent "player1"
        rest `shouldBe` ""
        pos `shouldBe` Position 1 8

      it "treats case-sensitive keywords as identifiers" $ do
        let (tok, rest, pos) = scanIdentifier "LET" (Position 1 1)
        tokenType tok `shouldBe` TIdent "LET"
        rest `shouldBe` ""
        pos `shouldBe` Position 1 4

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

      it "scans an empty string literal" $ do
        let (tok, rest, pos) = scanString "\"\"" (Position 1 1)
        tokenType tok `shouldBe` TString ""
        rest `shouldBe` ""
        pos `shouldBe` Position 1 3

      it "scans a string with an escaped quote" $ do
        let (tok, rest, pos) = scanString "\"a\\\"b\"" (Position 1 1)
        tokenType tok `shouldBe` TString "a\"b"
        rest `shouldBe` ""
        pos `shouldBe` Position 1 7

      it "handles escape sequences" $ do
        let (tok, rest, pos) = scanString "\"\\n\\t\\\\\"" (Position 1 1)
        tokenType tok `shouldBe` TString "\n\t\\"
        rest `shouldBe` ""
        pos `shouldBe` Position 1 9

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

      it "prefers two-character operators" $ do
        let (tok, rest, pos) = scanOperator "== " (Position 1 1)
        tokenType tok `shouldBe` TEq
        rest `shouldBe` " "
        pos `shouldBe` Position 1 3

      it "scans the modulo operator" $ do
        let (tok, rest, pos) = scanOperator "%" (Position 1 1)
        tokenType tok `shouldBe` TMod
        rest `shouldBe` ""
        pos `shouldBe` Position 1 2

      it "scans all operators" $ do
        let (tok1, _, _) = scanOperator "- " (Position 1 1)
        let (tok2, _, _) = scanOperator "/ " (Position 1 1)
        let (tok3, _, _) = scanOperator "< " (Position 1 1)
        let (tok4, _, _) = scanOperator "!= " (Position 1 1)
        let (tok5, _, _) = scanOperator "<= " (Position 1 1)
        let (tok6, _, _) = scanOperator "! " (Position 1 1)
        tokenType tok1 `shouldBe` TMinus
        tokenType tok2 `shouldBe` TDivide
        tokenType tok3 `shouldBe` TLt
        tokenType tok4 `shouldBe` TNotEqual
        tokenType tok5 `shouldBe` TLe
        tokenType tok6 `shouldBe` TNot

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

      it "scans braces" $ do
        let (tok, rest, pos) = scanSymbol "{}" (Position 1 1)
        tokenType tok `shouldBe` TBraceL
        rest `shouldBe` "}"
        pos `shouldBe` Position 1 2
        let (tok2, rest2, pos2) = scanSymbol rest pos
        tokenType tok2 `shouldBe` TBraceR
        rest2 `shouldBe` ""
        pos2 `shouldBe` Position 1 3

      it "scans brackets" $ do
        let (tok, rest, pos) = scanSymbol "[]" (Position 1 1)
        tokenType tok `shouldBe` TBracketL
        rest `shouldBe` "]"
        pos `shouldBe` Position 1 2
        let (tok2, rest2, pos2) = scanSymbol rest pos
        tokenType tok2 `shouldBe` TBracketR
        rest2 `shouldBe` ""
        pos2 `shouldBe` Position 1 3

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

      it "handles mixed whitespace" $ do
        let (rest, pos) = skipWhitespace " \t \t " (Position 1 1)
        rest `shouldBe` ""
        pos `shouldBe` Position 1 6

      it "handles whitespace at the end of a file" $ do
        let (rest, pos) = skipWhitespace "   " (Position 1 1)
        rest `shouldBe` ""
        pos `shouldBe` Position 1 4

  describe "Lexer Logic" $ do
    describe "Whitespace and Comments" $ do
      it "handles a single newline" $ do
        let tokens = lexer "let\nx"
        map tokenType tokens `shouldBe` [TKwLet, TNewline, TIdent "x", TEOF]
        map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 4, Position 2 1, Position 2 2]

      it "collapses multiple newlines into a blank" $ do
        let tokens = lexer "let\n\nx"
        map tokenType tokens `shouldBe` [TKwLet, TBlank, TIdent "x", TEOF]

      it "does not emit extra tokens for blank lines" $ do
        let tokens = lexer "let health = 100\n  \t\n\nlet score = 0\n"
        map tokenType tokens `shouldBe`
          [ TKwLet, TIdent "health", TAssign, TInt 100, TBlank, TNewline
          , TKwLet, TIdent "score", TAssign, TInt 0, TNewline, TEOF
          ]

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

      it "handles blank lines" $ do
        let tokens = lexer "let x = 5\n\nlet y = 6"
        map tokenType tokens `shouldBe` [TKwLet, TIdent "x", TAssign, TInt 5, TBlank, TKwLet, TIdent "y", TAssign, TInt 6, TEOF]
        map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 5, Position 1 7, Position 1 9, Position 1 10, Position 3 1, Position 3 5, Position 3 7, Position 3 9, Position 3 10]

    describe "General Lexing" $ do
      it "emits a separator after each completed statement" $ do
        let tokens = lexer "let health = 100\nlet score = 0\n"
        map tokenType tokens `shouldBe`
          [ TKwLet, TIdent "health", TAssign, TInt 100, TNewline
          , TKwLet, TIdent "score", TAssign, TInt 0, TNewline, TEOF
          ]

      it "separates tokens with newlines" $ do
        let tokens = lexer "1\n2"
        map tokenType tokens `shouldBe` [TInt 1, TNewline, TInt 2, TEOF]

      it "scans boolean literals" $ do
        let tokens = lexer "true false"
        map tokenType tokens `shouldBe` [TBool True, TBool False, TEOF]

      it "scans all keywords" $ do
        let tokens = lexer "else elif collision update interact"
        map tokenType tokens `shouldBe` [TKwElse, TKwElif, TKwCollision, TKwUpdate, TKwInteract, TEOF]

    describe "Error Handling" $ do
      it "reports an invalid character" $ do
        let tokens = lexer "let x = 1 ~ 2"
        let errorToken = head $ filter isError tokens
        tokenType errorToken `shouldBe` TError "Unexpected character: ~"
        tokenPosition errorToken `shouldBe` Position 1 11

      it "reports an unknown symbol" $ do
        let tokens = lexer "let x = @"
        let errorToken = head $ filter isError tokens
        tokenType errorToken `shouldBe` TError "Unexpected character: @"
        tokenPosition errorToken `shouldBe` Position 1 9

      it "handles an unterminated string" $ do
        let tokens = lexer "\"hello"
        let errorToken = head $ filter isError tokens
        tokenType errorToken `shouldBe` TError "Unterminated string literal"
        tokenPosition errorToken `shouldBe` Position 1 1

      it "reports an unknown operator" $ do
        let tokens = lexer "&"
        let errorToken = head $ filter isError tokens
        tokenType errorToken `shouldBe` TError "Unexpected character: &"
        tokenPosition errorToken `shouldBe` Position 1 1

      it "handles unsupported characters" $ do
        let tokens = lexer "😀"
        let errorToken = head $ filter isError tokens
        tokenType errorToken `shouldBe` TError "Unexpected character: 😀"
        tokenPosition errorToken `shouldBe` Position 1 1

      it "reports an invalid identifier" $ do
        let tokens = lexer "let $x = 5"
        let errorToken = head $ filter isError tokens
        tokenType errorToken `shouldBe` TError "Unexpected character: $"
        tokenPosition errorToken `shouldBe` Position 1 5

      it "handles multiline strings" $ do
        let tokens = lexer "\"hello\nworld\""
        let errorToken = head $ filter isError tokens
        tokenType errorToken `shouldBe` TError "Newline in string"
        tokenPosition errorToken `shouldBe` Position 1 1

    describe "Edge Cases" $ do
      it "handles empty input" $ do
        let tokens = lexer ""
        map tokenType tokens `shouldBe` [TEOF]

      it "handles input with only whitespace" $ do
        let tokens = lexer "  \t  "
        map tokenType tokens `shouldBe` [TEOF]
        tokenPosition (head tokens) `shouldBe` Position 1 6

      it "handles CRLF line endings" $ do
        let tokens = lexer "let x = 1\r\nlet y = 2"
        map tokenType tokens `shouldBe` [TKwLet, TIdent "x", TAssign, TInt 1, TNewline, TKwLet, TIdent "y", TAssign, TInt 2, TEOF]
        map tokenPosition (filter ((== 2) . line . tokenPosition) tokens) `shouldStartWith` [Position 2 1]

      it "handles unexpected end-of-file" $ do
        let tokens = lexer "let x ="
        map tokenType tokens `shouldBe` [TKwLet, TIdent "x", TAssign, TEOF]

      it "handles token concatenation" $ do
        let tokens = lexer "123let"
        map tokenType tokens `shouldBe` [TInt 123, TKwLet, TEOF]

  describe "Golden Tests" $ do
    it "parses a simple variable declaration" $ do
      let tokens = lexer "let score = 5"
      map tokenType tokens `shouldBe` [TKwLet, TIdent "score", TAssign, TInt 5, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 5, Position 1 11, Position 1 13, Position 1 14]

    it "parses a simple assignment" $ do
      let tokens = lexer "score = score + 5"
      map tokenType tokens `shouldBe` [TIdent "score", TAssign, TIdent "score", TPlus, TInt 5, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 7, Position 1 9, Position 1 15, Position 1 17, Position 1 18]

    it "parses arithmetic operations" $ do
      let tokens = lexer "x = 5 + 2 * 8"
      map tokenType tokens `shouldBe` [TIdent "x", TAssign, TInt 5, TPlus, TInt 2, TMultiply, TInt 8, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 3, Position 1 5, Position 1 7, Position 1 9, Position 1 11, Position 1 13, Position 1 14]

    it "parses a simple event handler" $ do
      let tokens = lexer "on key_press SPACE:\n{\nplayer.jump()\n}"
      map tokenType tokens `shouldBe` [TKwOn, TKwKeyPress, TIdent "SPACE", TColon, TNewline, TBraceL, TNewline, TIdent "player", TDot, TIdent "jump", TLParen, TRParen, TNewline, TBraceR, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 4, Position 1 14, Position 1 19, Position 1 20, Position 2 1, Position 2 2, Position 3 1, Position 3 7, Position 3 8, Position 3 12, Position 3 13, Position 3 14, Position 4 1, Position 4 2]

    it "parses a simple if statement" $ do
      let tokens = lexer "if score > 10:\n{\nenemy.attack()\n}"
      map tokenType tokens `shouldBe` [TKwIf, TIdent "score", TGt, TInt 10, TColon, TNewline, TBraceL, TNewline, TIdent "enemy", TDot, TIdent "attack", TLParen, TRParen, TNewline, TBraceR, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 4, Position 1 10, Position 1 12, Position 1 14, Position 1 15, Position 2 1, Position 2 2, Position 3 1, Position 3 6, Position 3 7, Position 3 13, Position 3 14, Position 3 15, Position 4 1, Position 4 2]

    it "parses a function call with a string argument" $ do
      let tokens = lexer "dialogue.show(\"Hello\")"
      map tokenType tokens `shouldBe` [TIdent "dialogue", TDot, TIdent "show", TLParen, TString "Hello", TRParen, TEOF]
      map tokenPosition tokens `shouldBe` [Position 1 1, Position 1 9, Position 1 10, Position 1 14, Position 1 15, Position 1 22, Position 1 23]