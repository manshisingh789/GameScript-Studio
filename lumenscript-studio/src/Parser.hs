module Parser
  ( parseProgram
  , parseStatement
  , parseBlock
  , parseDeclaration
  , parseAssignmentOrExprStatement
  , parseIfStatement
  , parseEventStatement
  ) where
import Token
import AST
import ParserTypes
import ParserExpressions (parseExpression)

-- program = { statement } ;
parseProgram :: [Token] -> ParseResult Program
parseProgram ts = go (skipNewlines ts) []
  where
    go rest acc = case peek rest of
      Nothing -> pure (reverse acc, rest)
      Just t | tokenType t == TEOF -> pure (reverse acc, rest)
      _ -> do
        (stmt, rest') <- parseStatement rest
        go (skipSemicolons (skipNewlines rest')) (stmt:acc)

    skipSemicolons ts = case peek ts of
      Just t | tokenType t == TSemicolon -> skipSemicolons (drop 1 ts)
      _ -> ts
-- statement = declaration | assignment | eventStatement | ifStatement | functionCall ;
parseStatement :: [Token] -> ParseResult Stmt
parseStatement [] = Left (ParseError "Unexpected end of input while parsing statement" eofPosition)
parseStatement ts@(t:_) = case tokenType t of
  TKwLet -> parseDeclaration ts
  TKwIf  -> parseIfStatement ts
  TKwOn  -> parseEventStatement ts
  TIdent _ -> parseAssignmentOrExprStatement ts
  other -> Left (ParseError ("Unexpected token at start of statement: " ++ show other) (tokenPosition t))
-- declaration = "let" identifier "=" expression ;
parseDeclaration :: [Token] -> ParseResult Stmt
parseDeclaration (kw:rest0)
  | tokenType kw == TKwLet = case rest0 of
      (idTok:eqTok:rest1)
        | TIdent name <- tokenType idTok
        , tokenType eqTok == TAssign -> do
            (rhs, rest2) <- parseExpression rest1
            pure (Decl name rhs (tokenPosition idTok), rest2)
      (t:_) -> Left (ParseError "Malformed 'let' declaration: expected identifier '=' expression" (tokenPosition t))
      [] -> Left (ParseError "Unexpected end of input in 'let' declaration" eofPosition)
parseDeclaration (t:_) = Left (ParseError "Expected 'let'" (tokenPosition t))
parseDeclaration [] = Left (ParseError "Unexpected end of input" eofPosition)
-- assignment    = identifier "=" expression ;
-- functionCall  = member "(" [ argumentList ] ")" ;   (as a statement)
--
-- Both start with an identifier, so peek ahead: if it's immediately
-- followed by '=', treat it as an assignment. Otherwise hand the
-- whole thing to parseExpression, which already builds member/call
-- chains -- the result must be a Call to be a valid statement (per
-- the grammar, statement's only expression-shaped alternative is
-- functionCall, not bare expressions like "x + 1" on their own line).
parseAssignmentOrExprStatement :: [Token] -> ParseResult Stmt
parseAssignmentOrExprStatement ts = do
  (expr, rest) <- parseExpression ts
  case rest of
    (t:rest') | tokenType t == TAssign -> do
      (rhs, rest'') <- parseExpression rest'
      case expr of
        Var _ pos -> pure (Assign expr rhs pos, rest'')
        Member _ _ pos -> pure (Assign expr rhs pos, rest'')
        _ -> Left (ParseError "Invalid assignment target" (case ts of (t':_) -> tokenPosition t'; _ -> eofPosition))
    _ -> pure (ExprStmt expr, rest)
-- ifStatement = "if" expression ":" block [ "else" ":" block ] ;
parseIfStatement :: [Token] -> ParseResult Stmt
parseIfStatement (kw:rest0)
  | tokenType kw == TKwIf = do
      (cond, rest1) <- parseExpression rest0
      (thenBlock, rest2) <- case peek rest1 of
        Just t | tokenType t == TColon -> do
          (block, rest) <- parseBlock (drop 1 rest1)
          return (block, rest)
        _ -> parseBlock rest1
      case skipNewlines rest2 of
        (elseTok:rest3) | tokenType elseTok == TKwElse -> do
          (elseBlock, rest4) <- case peek rest3 of
            Just t | tokenType t == TColon -> do
              (block, rest) <- parseBlock (drop 1 rest3)
              return (block, rest)
            _ -> parseBlock rest3
          pure (If cond thenBlock (Just elseBlock) (tokenPosition kw), rest4)
        _ -> pure (If cond thenBlock Nothing (tokenPosition kw), rest2)
parseIfStatement (t:_) = Left (ParseError "Expected 'if'" (tokenPosition t))
parseIfStatement [] = Left (ParseError "Unexpected end of input" eofPosition)
-- eventStatement = "on" "key_press" identifier ":" block ;
parseEventStatement :: [Token] -> ParseResult Stmt
parseEventStatement (onTok:kpTok:idTok:colonTok:rest)
  | tokenType onTok == TKwOn
  , tokenType kpTok == TKwKeyPress
  , TIdent name <- tokenType idTok
  , tokenType colonTok == TColon = do
      (body, rest') <- parseBlock rest
      pure (OnEvent (KeyPress name) body (tokenPosition onTok), rest')
parseEventStatement (t:_) = Left (ParseError "Malformed event statement: expected 'on key_press <identifier> :'" (tokenPosition t))
parseEventStatement [] = Left (ParseError "Unexpected end of input in event statement" eofPosition)
-- block = "{" { statement } "}" ;
parseBlock :: [Token] -> ParseResult [Stmt]
parseBlock ts = do
  rest0 <- expect TBraceL (skipNewlines ts)
  go (skipNewlines rest0) []
  where
    go rest acc = case peek rest of
      Just t | tokenType t == TBraceR -> pure (reverse acc, drop 1 rest)
      Just t | tokenType t == TEOF -> Left (ParseError "Unterminated block: missing '}'" (tokenPosition t))
      Nothing -> Left (ParseError "Unterminated block: missing '}'" eofPosition)
      _ -> do
        (stmt, rest') <- parseStatement rest
        go (skipSemicolons (skipNewlines rest')) (stmt:acc)

    skipSemicolons ts = case peek ts of
      Just t | tokenType t == TSemicolon -> skipSemicolons (drop 1 ts)
      _ -> ts