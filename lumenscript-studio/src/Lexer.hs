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
      newPos         = advanceBy (length digits) pos
  in (Token payload pos, rest, newPos)

-- Utility: advance position by N columns
advanceBy :: Int -> Position -> Position
advanceBy n (Position l c) = Position l (c + n)

isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/%=<>!"

isSymbol :: Char -> Bool
isSymbol c = c `elem` "(){}[],.;:"

-- Scan an identifier or keyword
scanIdentifier :: String -> Position -> LexResult
scanIdentifier input pos =
  let (ident, rest) = span isIdentChar input
      payload       = lookupKeyword ident
      newPos        = advanceBy (length ident) pos
  in (Token payload pos, rest, newPos)

-- Utility: what counts as identifier characters
isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || isDigit c || c == '_'

-- Check if an identifier is a keyword
lookupKeyword :: String -> TokenPayload
lookupKeyword ident = fromMaybe (TIdent ident) (lookup ident keywords)

scanString :: String -> Position -> LexResult
scanString (_:input) pos = -- We skip the opening quote
  let (content, rest) = span (/= '"') input
  in if null rest
      then (Token (TError "Unterminated string") pos, "", pos)
      else let newPos = advanceBy (length content + 2) pos -- +2 for the quotes
           in (Token (TString content) pos, tail rest, newPos)

scanOperator :: String -> Position -> LexResult
scanOperator (c1:c2:cs) pos
  | [c1,c2] == "==" = (Token TEq pos, cs, advanceBy 2 pos)
  | [c1,c2] == "!=" = (Token TNotEqual pos, cs, advanceBy 2 pos)
  | [c1,c2] == "<=" = (Token TLe pos, cs, advanceBy 2 pos)
  | [c1,c2] == ">=" = (Token TGe pos, cs, advanceBy 2 pos)
scanOperator (c:cs) pos
  | c == '+' = (Token TPlus pos, cs, advanceBy 1 pos)
  | c == '-' = (Token TMinus pos, cs, advanceBy 1 pos)
  | c == '*' = (Token TMultiply pos, cs, advanceBy 1 pos)
  | c == '/' = (Token TDivide pos, cs, advanceBy 1 pos)
  | c == '%' = (Token TMod pos, cs, advanceBy 1 pos)
  | c == '<' = (Token TLt pos, cs, advanceBy 1 pos)
  | c == '>' = (Token TGt pos, cs, advanceBy 1 pos)
  | c == '=' = (Token TAssign pos, cs, advanceBy 1 pos)
scanOperator s pos = (Token (TError ("Invalid operator: " ++ s)) pos, s, pos)

scanSymbol :: String -> Position -> LexResult
scanSymbol (c:cs) pos
  | c == '(' = (Token TLParen pos, cs, advanceBy 1 pos)
  | c == ')' = (Token TRParen pos, cs, advanceBy 1 pos)
  | c == '.' = (Token TDot pos, cs, advanceBy 1 pos)
  | c == ',' = (Token TComma pos, cs, advanceBy 1 pos)
  | c == ':' = (Token TColon pos, cs, advanceBy 1 pos)
scanSymbol s pos = (Token (TError ("Invalid symbol: " ++ s)) pos, s, pos)

skipWhitespace :: String -> Position -> (String, Position)
skipWhitespace input pos =
  let (ws, rest) = span isWhitespaceChar input
      newPos     = advanceBy (length ws) pos
  in (rest, newPos)

isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c == ' ' || c == '\t'

skipComment :: String -> Position -> (String, Position)
skipComment input pos =
  let (comment, rest) = span (/= '\n') input
      newPos          = advanceBy (length comment) pos
  in (rest, newPos)

emitNewline :: String -> Position -> (String, Position)
emitNewline input pos =
  let (newlines, rest) = span (== '\n') input
      lineCount        = length newlines
      newPos           = pos { line = line pos + lineCount, column = 1 }
  in (rest, newPos)

advancePosition :: Position -> Position
advancePosition pos = advanceBy 1 pos

lexer :: String -> [Token]
lexer input = reverse $ go input initialPos []
  where
    go :: String -> Position -> [Token] -> [Token]
    go [] pos acc =
      Token TEOF pos : acc   -- eof

    go (c:cs) pos acc =
      case c of
        _ | isDigit c      -> let (tok, rest, pos') = scanNumber (c:cs) pos
                              in go rest pos' (tok : acc)
          | isLetter c     -> let (tok, rest, pos') = scanIdentifier (c:cs) pos
                              in go rest pos' (tok : acc)
          | c == '"'       -> let (tok, rest, pos') = scanString (c:cs) pos
                              in go rest pos' (tok : acc)
          | isOperator c   -> let (tok, rest, pos') = scanOperator (c:cs) pos
                              in go rest pos' (tok : acc)
          | isSymbol c     -> let (tok, rest, pos') = scanSymbol (c:cs) pos
                              in go rest pos' (tok : acc)
          | c == '\n'      -> let (rest, pos') = emitNewline (c:cs) pos
                              in go rest pos' (Token TNewline pos : acc)
          | isWhitespaceChar c      -> let (rest, pos') = skipWhitespace (c:cs) pos
                              in go rest pos' acc
          | c == '#'       -> let (rest, pos') = skipComment (c:cs) pos
                              in go rest pos' acc
          | otherwise      -> go cs (advancePosition pos) (Token (TError ("Invalid character: " ++ [c])) pos : acc)


initialPos :: Position
initialPos = Position { line = 1, column = 1 }
