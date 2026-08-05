module ParserSpec (spec) where

import Test.Hspec
import Token
import AST
import ParserExpressions

-- Small helper: build a token at a throwaway position, since these
-- tests only care about token *type*, not exact column tracking.
tok :: TokenPayload -> Token
tok tp = Token tp (Position 1 1)

spec :: Spec
spec = describe "ParserExpressions" $ do

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