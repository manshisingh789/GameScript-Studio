module ParserExpressions
  ( parseExpression
  , parseComparison
  , parseAddition
  , parseMultiplication
  , parseUnary
  , parsePrimary
  , parseArgumentList
  ) where

import Token
import AST
import ParserTypes

-- expression = comparison ;
parseExpression :: [Token] -> ParseResult Expr
parseExpression = parseComparison

-- comparison = addition [ comparisonOperator addition ] ;
parseComparison :: [Token] -> ParseResult Expr
parseComparison ts = do
  (lhs, rest) <- parseAddition ts
  case peek rest of
    Just t | Just op <- comparisonOp (tokenType t) -> do
      (rhs, rest') <- parseAddition (drop 1 rest)
      pure (Binary op lhs rhs, rest')
    _ -> pure (lhs, rest)
  where
    comparisonOp TEq       = Just Eq
    comparisonOp TNotEqual = Just NotEq
    comparisonOp TLt       = Just Lt
    comparisonOp TGt       = Just Gt
    comparisonOp TLe       = Just Le
    comparisonOp TGe       = Just Ge
    comparisonOp _         = Nothing

-- addition = multiplication { additionOperator multiplication } ;
parseAddition :: [Token] -> ParseResult Expr
parseAddition ts = do
  (lhs, rest) <- parseMultiplication ts
  go lhs rest
  where
    go acc rest = case peek rest of
      Just t | Just op <- additionOp (tokenType t) -> do
        (rhs, rest') <- parseMultiplication (drop 1 rest)
        go (Binary op acc rhs) rest'
      _ -> pure (acc, rest)
    additionOp TPlus  = Just Add
    additionOp TMinus = Just Sub
    additionOp _      = Nothing

-- multiplication = unary { multiplicationOperator unary } ;
parseMultiplication :: [Token] -> ParseResult Expr
parseMultiplication ts = do
  (lhs, rest) <- parseUnary ts
  go lhs rest
  where
    go acc rest = case peek rest of
      Just t | Just op <- mulOp (tokenType t) -> do
        (rhs, rest') <- parseUnary (drop 1 rest)
        go (Binary op acc rhs) rest'
      _ -> pure (acc, rest)
    mulOp TMultiply = Just Mul
    mulOp TDivide   = Just Div
    mulOp TMod      = Just Mod
    mulOp _         = Nothing

-- unary = [ "-" | "!" ] primary ;
parseUnary :: [Token] -> ParseResult Expr
parseUnary (t:ts)
  | tokenType t == TMinus = do
      (operand, rest) <- parseUnary ts
      pure (Unary Neg operand, rest)
  | tokenType t == TNot = do
      (operand, rest) <- parseUnary ts
      pure (Unary Not operand, rest)
parseUnary ts = parsePrimary ts

-- primary      = integer | string | boolean | identifier | member | "(" expression ")" ;
-- member       = identifier "." identifier ;
-- functionCall = member "(" [ argumentList ] ")" ;
--
-- Note: the lexer already folds negative-number literals (e.g. "-5")
-- into a single TInt token (see scanNumber in Lexer.hs), so unary
-- minus here mainly applies to expressions like -(x + 1) or -x where
-- x is an identifier, not to raw numeric literals.
--
-- identifier/member/functionCall are parsed together as one chain:
-- take a name, extend with ".field" while dots follow, then
-- optionally apply a trailing "(args)" call. Because Call now takes
-- an Expr (not two Idents), calls can appear inside expressions, e.g.
-- let x = player.getHealth()
parsePrimary :: [Token] -> ParseResult Expr
parsePrimary [] = Left (ParseError "Unexpected end of input" eofPosition)
parsePrimary (t:ts) = case tokenType t of
  TInt n       -> pure (LitInt n, ts)
  TString s    -> pure (LitStr s, ts)
  TBool b      -> pure (LitBool b, ts)
  TIdent name  -> parseCallChain (Var name) ts
  TLParen      -> do
    (inner, rest) <- parseExpression ts
    rest' <- expect TRParen rest
    pure (inner, rest')
  other -> Left (ParseError ("Unexpected token in expression: " ++ show other) (tokenPosition t))

-- Extends a base expression with ".field" accesses and/or a trailing
-- "(args)" call, e.g.  player . jump ( )  ->  Call (Member (Var "player") "jump") []
parseCallChain :: Expr -> [Token] -> ParseResult Expr
parseCallChain base ts = do
  (afterMembers, rest) <- consumeMembers base ts
  case peek rest of
    Just t | tokenType t == TLParen -> do
      (args, rest') <- parseArgumentList (drop 1 rest)
      pure (Call afterMembers args, rest')
    _ -> pure (afterMembers, rest)
  where
    consumeMembers acc (dotTok:fieldTok:more)
      | tokenType dotTok == TDot
      , TIdent field <- tokenType fieldTok
      = consumeMembers (Member acc field) more
    consumeMembers _ (dotTok:_)
      | tokenType dotTok == TDot
      = Left (ParseError "Expected identifier after '.'" (tokenPosition dotTok))
    consumeMembers acc rest = Right (acc, rest)

-- argumentList = expression { "," expression } ;
-- Called with the tokens right after "(". Handles the empty-args case
-- and consumes the closing ")" itself.
parseArgumentList :: [Token] -> ParseResult [Expr]
parseArgumentList ts = case peek ts of
  Just t | tokenType t == TRParen -> pure ([], drop 1 ts)
  _ -> do
    (first, rest) <- parseExpression ts
    go [first] rest
  where
    go acc rest = case peek rest of
      Just t | tokenType t == TComma -> do
        (next, rest') <- parseExpression (drop 1 rest)
        go (next:acc) rest'
      Just t | tokenType t == TRParen -> pure (reverse acc, drop 1 rest)
      Just t -> Left (ParseError ("Expected ',' or ')' in argument list, got " ++ show (tokenType t)) (tokenPosition t))
      Nothing -> Left (ParseError "Unterminated argument list" eofPosition)