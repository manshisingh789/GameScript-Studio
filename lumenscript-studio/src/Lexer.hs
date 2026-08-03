module Lexer where

import Data.Char (isDigit, isLetter)
import Token
import Data.Maybe (fromMaybe)

type LexResult = (Token, String, Position)

-- Scan a sequence of digits into an integer token
scanNumber :: String -> Position -> LexResult
scanNumber input pos =
  let (digits, rest) = span isDigit input
      payload        = TInt (read digits)
      newPos         = foldl advanceChar pos digits
  in (Token payload pos, rest, newPos)

isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/%=<>!"

-- NOTE: semicolon removed from the symbol set
isSymbol :: Char -> Bool
isSymbol c = c `elem` "(){}[],.:"

-- Scan an identifier or keyword
scanIdentifier :: String -> Position -> LexResult
scanIdentifier input pos =
  let (ident, rest) = span isIdentChar input
      payload       = lookupKeyword ident
      newPos        = foldl advanceChar pos ident
  in (Token payload pos, rest, newPos)

-- Utility: what counts as identifier characters
isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || isDigit c || c == '_'

-- Check if an identifier is a keyword `keywords` is expected to be exported from Token; verify it contains
-- let, if, else, on, key_press, true, false
lookupKeyword :: String -> TokenPayload
lookupKeyword ident = fromMaybe (TIdent ident) (lookup ident keywords)

-- Scan a string literal, supporting \" \\ \n \t escapes
scanString :: String -> Position -> LexResult
scanString (_:input) pos = -- skip the opening quote
  go input (advanceChar pos '"') []
  where
    go ('\\':c:rest) currentPos content =
      let newPos = foldl advanceChar currentPos ['\\', c]
      in case escapeChar c of
        Just ec -> go rest newPos (ec : content)
        Nothing -> go rest newPos (c : content) -- unknown escape: keep literal char
    go ('\\':[]) currentPos content =
      (Token (TError "Unterminated string") pos, "", advanceChar currentPos '\\')
    go ('"':rest) currentPos content =
      let payload = TString (reverse content)
          newPos  = advanceChar currentPos '"'
      in (Token payload pos, rest, newPos)
    go ('\n':cs) currentPos _ =
      (Token (TError "Newline in string") pos, cs, currentPos)
    go (c:cs) currentPos content =
      go cs (advanceChar currentPos c) (c : content)
    go "" endPos _ =
      (Token (TError "Unterminated string") pos, "", endPos)
scanString "" pos = (Token (TError "Unexpected end of file") pos, "", pos)

-- Map recognized escape characters to their literal value
escapeChar :: Char -> Maybe Char
escapeChar '"'  = Just '"'
escapeChar '\\' = Just '\\'
escapeChar 'n'  = Just '\n'
escapeChar 't'  = Just '\t'
escapeChar _    = Nothing

scanOperator :: String -> Position -> LexResult
scanOperator [] pos = (Token (TError "Unexpected end of file") pos, "", pos)
scanOperator input@(c1:cs) pos =
    case input of
        (c1:c2:rest) -> case lookup [c1,c2] multiCharOps of
                            Just payload -> (Token payload pos, rest, foldl advanceChar pos [c1, c2])
                            Nothing -> single
        _ -> single
  where
    single = case lookup c1 singleCharOps of
               Just payload -> (Token payload pos, cs, advanceChar pos c1)
               Nothing -> (Token (TError ("Invalid operator: " ++ [c1])) pos, cs, pos)

multiCharOps :: [(String, TokenPayload)]
multiCharOps = [("==", TEq), ("!=", TNotEqual), ("<=", TLe), (">=", TGe)]

-- Added '!' for standalone unary not
singleCharOps :: [(Char, TokenPayload)]
singleCharOps = [('+', TPlus), ('-', TMinus), ('*', TMultiply), ('/', TDivide),
                 ('%', TMod), ('<', TLt), ('>', TGt), ('=', TAssign),
                 ('!', TNot)]

scanSymbol :: String -> Position -> LexResult
scanSymbol (c:cs) pos =
  case lookup c symbolMap of
    Just payload -> (Token payload pos, cs, advanceChar pos c)
    Nothing      -> (Token (TError ("Invalid symbol: " ++ [c])) pos, cs, pos)
scanSymbol "" pos = (Token (TError "Unexpected end of file") pos, "", pos)

-- Semicolon entry removed
symbolMap :: [(Char, TokenPayload)]
symbolMap = [
    ('(', TLParen), (')', TRParen),
    ('{', TBraceL), ('}', TBraceR),
    ('[', TBracketL), (']', TBracketR),
    (',', TComma), ('.', TDot),
    (':', TColon)
  ]

-- Whitespace now includes '\n' so newlines are skipped rather than
-- turned into TNewline tokens; line/column tracking still works via
-- advanceChar's '\n' case.
isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c `elem` " \t\r"

skipWhitespace :: String -> Position -> (String, Position)
skipWhitespace input pos =
  let (ws, rest) = span isWhitespaceChar input
      newPos     = foldl advanceChar pos ws
  in (rest, newPos)

skipComment :: String -> Position -> (String, Position)
skipComment input pos =
  let (comment, rest) = span (/= '\n') input
      newPos          = foldl advanceChar pos comment
  in (rest, newPos)

lexer :: String -> [Token]
lexer input = postProcessTokens $ reverse $ go input initialPos []
  where
    go :: String -> Position -> [Token] -> [Token]
    go "" pos acc = Token TEOF pos : acc
    go str pos acc =
        let (maybeTok, rest, pos') = lexAtom str pos
        in case maybeTok of
            Just tok -> go rest pos' (tok : acc)
            Nothing  -> go rest pos' acc

postProcessTokens :: [Token] -> [Token]
postProcessTokens [] = []
postProcessTokens (t1:t2:ts)
  | isDoubleNewline t1 t2 = postProcessTokens (Token TBlank (tokenPosition t1) : ts)
  | otherwise = t1 : postProcessTokens (t2:ts)
postProcessTokens (t:ts) = t : postProcessTokens ts

isDoubleNewline :: Token -> Token -> Bool
isDoubleNewline t1 t2 = tokenType t1 == TNewline && tokenType t2 == TNewline

type LexAtomResult = (Maybe Token, String, Position)

lexAtom :: String -> Position -> LexAtomResult
lexAtom "" pos = (Just (Token TEOF pos), "", pos)
lexAtom (c:cs) pos
  | c == '\n' = (Just (Token TNewline pos), cs, advanceChar pos '\n')
  | isWhitespaceChar c = let (rest, pos') = skipWhitespace (c:cs) pos
                          in (Nothing, rest, pos')
  | isDigit c          = let (tok, rest, pos') = scanNumber (c:cs) pos in (Just tok, rest, pos')
  | isLetter c         = let (tok, rest, pos') = scanIdentifier (c:cs) pos in (Just tok, rest, pos')
  | c == '"'           = let (tok, rest, pos') = scanString (c:cs) pos in (Just tok, rest, pos')
  | isOperator c       = let (tok, rest, pos') = scanOperator (c:cs) pos in (Just tok, rest, pos')
  | isSymbol c         = let (tok, rest, pos') = scanSymbol (c:cs) pos in (Just tok, rest, pos')
  | c == '#'           = let (rest, pos') = skipComment cs (advanceChar pos '#') in (Nothing, rest, pos')
  | otherwise          = (Just (Token TInvalid pos), cs, advanceChar pos c)

advanceChar :: Position -> Char -> Position
advanceChar pos '\n' = pos { line = line pos + 1, column = 1 }
advanceChar pos _    = pos { column = column pos + 1 }

initialPos :: Position
initialPos = Position { line = 1, column = 1 }