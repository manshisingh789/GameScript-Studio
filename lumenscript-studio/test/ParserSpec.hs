module ParserSpec (spec) where

import Test.Hspec
import Token
import AST
import ParserExpressions
import Parser

tok :: TokenPayload -> Token
tok tp = Token tp (Position 1 1)

spec :: Spec
spec = do
  describe "ParserExpressions" $ do

    it "parses a simple integer literal" $ do
      let toks = [tok (TInt 42), tok TEOF]
      parseExpression toks `shouldBe` Right (LitInt 42, [tok TEOF])

    it "parses member access" $ do
      let toks = [tok (TIdent "player"), tok TDot, tok (TIdent "health"), tok TEOF]
      parseExpression toks
        `shouldBe` Right (Member (Var "player") "health", [tok TEOF])

    it "parses addition with member access" $ do
      let toks = [ tok (TIdent "player"), tok TDot, tok (TIdent "health")
                 , tok TPlus, tok (TInt 5), tok TEOF
                 ]
      parseExpression toks
        `shouldBe` Right (Binary Add (Member (Var "player") "health") (LitInt 5), [tok TEOF])

    it "parses a comparison" $ do
      let toks = [tok (TInt 5), tok TLe, tok (TInt 10), tok TEOF]
      parseExpression toks
        `shouldBe` Right (Binary Le (LitInt 5) (LitInt 10), [tok TEOF])

    it "parses a zero-argument function call" $ do
      let toks = [ tok (TIdent "player"), tok TDot, tok (TIdent "jump")
                 , tok TLParen, tok TRParen, tok TEOF
                 ]
      parseExpression toks
        `shouldBe` Right (Call (Member (Var "player") "jump") [], [tok TEOF])

    it "parses a function call with arguments" $ do
      let toks = [ tok (TIdent "dialogue"), tok TDot, tok (TIdent "show")
                 , tok TLParen, tok (TString "hi"), tok TComma, tok (TInt 1)
                 , tok TRParen, tok TEOF
                 ]
      parseExpression toks
        `shouldBe` Right (Call (Member (Var "dialogue") "show") [LitStr "hi", LitInt 1], [tok TEOF])

    it "parses standalone unary not" $ do
      let toks = [tok TNot, tok (TIdent "flag"), tok TEOF]
      parseExpression toks `shouldBe` Right (Unary Not (Var "flag"), [tok TEOF])

    it "parses unary minus on an identifier" $ do
      let toks = [tok TMinus, tok (TIdent "x"), tok TEOF]
      parseExpression toks `shouldBe` Right (Unary Neg (Var "x"), [tok TEOF])

    it "parses parenthesized expressions" $ do
      let toks = [ tok TLParen, tok (TInt 1), tok TPlus, tok (TInt 2), tok TRParen
                 , tok TMultiply, tok (TInt 3), tok TEOF
                 ]
      parseExpression toks
        `shouldBe` Right (Binary Mul (Binary Add (LitInt 1) (LitInt 2)) (LitInt 3), [tok TEOF])

    it "parses boolean literals" $ do
      let toks = [tok (TBool True), tok TEOF]
      parseExpression toks `shouldBe` Right (LitBool True, [tok TEOF])

    it "errors on a dangling dot" $ do
      let toks = [tok (TIdent "player"), tok TDot, tok TEOF]
      case parseExpression toks of
        Left _  -> pure ()
        Right r -> expectationFailure ("Expected a parse error, got: " ++ show r)

    it "errors on an unterminated argument list" $ do
      let toks = [ tok (TIdent "player"), tok TDot, tok (TIdent "jump")
                 , tok TLParen, tok (TInt 1)
                 ]
      case parseExpression toks of
        Left _  -> pure ()
        Right r -> expectationFailure ("Expected a parse error, got: " ++ show r)

  describe "Parser" $ do

    it "parses a let declaration" $ do
      let toks = [tok TKwLet, tok (TIdent "health"), tok TAssign, tok (TInt 100), tok TEOF]
      parseStatement toks `shouldBe` Right (Decl "health" (LitInt 100), [tok TEOF])

    it "parses an assignment" $ do
      let toks = [ tok (TIdent "health"), tok TAssign, tok (TIdent "health")
                 , tok TMinus, tok (TInt 10), tok TEOF
                 ]
      parseStatement toks
        `shouldBe` Right (Assign "health" (Binary Sub (Var "health") (LitInt 10)), [tok TEOF])

    it "parses a bare function-call statement" $ do
      let toks = [ tok (TIdent "player"), tok TDot, tok (TIdent "jump")
                 , tok TLParen, tok TRParen, tok TEOF
                 ]
      parseStatement toks
        `shouldBe` Right (ExprStmt (Call (Member (Var "player") "jump") []), [tok TEOF])

    it "rejects a bare non-call expression as a statement" $ do
      let toks = [tok (TInt 1), tok TPlus, tok (TInt 2), tok TEOF]
      case parseStatement toks of
        Left _  -> pure ()
        Right r -> expectationFailure ("Expected a parse error, got: " ++ show r)

    it "parses an if statement with no else" $ do
      let toks = [ tok TKwIf, tok (TIdent "health"), tok TLe, tok (TInt 0), tok TColon
                 , tok TBraceL
                 , tok (TIdent "dialogue"), tok TDot, tok (TIdent "show")
                 , tok TLParen, tok (TString "Game Over"), tok TRParen
                 , tok TBraceR, tok TEOF
                 ]
      parseStatement toks
        `shouldBe` Right
          ( If (Binary Le (Var "health") (LitInt 0))
               [ExprStmt (Call (Member (Var "dialogue") "show") [LitStr "Game Over"])]
               Nothing
          , [tok TEOF]
          )

    it "parses an if/else statement" $ do
      let toks = [ tok TKwIf, tok (TIdent "health"), tok TGt, tok (TInt 0), tok TColon
                 , tok TBraceL, tok TBraceR
                 , tok TKwElse, tok TColon
                 , tok TBraceL, tok TBraceR
                 , tok TEOF
                 ]
      parseStatement toks
        `shouldBe` Right (If (Binary Gt (Var "health") (LitInt 0)) [] (Just []), [tok TEOF])

    it "parses an event statement" $ do
      let toks = [ tok TKwOn, tok TKwKeyPress, tok (TIdent "SPACE"), tok TColon
                 , tok TBraceL
                 , tok (TIdent "player"), tok TDot, tok (TIdent "jump")
                 , tok TLParen, tok TRParen
                 , tok TBraceR, tok TEOF
                 ]
      parseStatement toks
        `shouldBe` Right
          ( OnEvent (KeyPress "SPACE")
              [ExprStmt (Call (Member (Var "player") "jump") [])]
          , [tok TEOF]
          )

    it "parses a full program with multiple statements" $ do
      let toks = [ tok TKwLet, tok (TIdent "score"), tok TAssign, tok (TInt 0)
                 , tok TKwOn, tok TKwKeyPress, tok (TIdent "SPACE"), tok TColon
                 , tok TBraceL
                 , tok (TIdent "player"), tok TDot, tok (TIdent "jump")
                 , tok TLParen, tok TRParen
                 , tok TBraceR
                 , tok TEOF
                 ]
      parseProgram toks
        `shouldBe` Right
          ( [ Decl "score" (LitInt 0)
            , OnEvent (KeyPress "SPACE") [ExprStmt (Call (Member (Var "player") "jump") [])]
            ]
          , [tok TEOF]
          )

    it "errors on an unterminated block" $ do
      let toks = [ tok TKwIf, tok (TBool True), tok TColon, tok TBraceL, tok TEOF ]
      case parseStatement toks of
        Left _  -> pure ()
        Right r -> expectationFailure ("Expected a parse error, got: " ++ show r)