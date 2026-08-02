module Lexer where

import Data.Char (isDigit, isLetter)
import Token

import Data.Maybe (fromMaybe)

-- Position tracking
data Position = Position { line :: Int, column :: Int }
  deriving (Show, Eq)

type LexResult = (Token, String, Position)

-- Scan a sequence of digits into an integer token
scanNumber :: String -> Position -> LexResult
scanNumber input pos =
  let (digits, rest) = span isDigit input
      token          = TInt (read digits)
      newPos         = advanceBy (length digits) pos
  in (token, rest, newPos)

-- Utility: advance position by N columns
advanceBy :: Int -> Position -> Position
advanceBy n (Position l c) = Position l (c + n)

isOperator :: Char -> Bool
isOperator c = c `elem` "+-*/%="

isSymbol :: Char -> Bool
isSymbol c = c `elem` "(){}[],;"


-- Scan an identifier or keyword
scanIdentifier :: String -> Position -> LexResult
scanIdentifier input pos =
  let (ident, rest) = span isIdentChar input
      token         = lookupKeyword ident
      newPos        = advanceBy (length ident) pos
  in (token, rest, newPos)

-- Utility: what counts as identifier characters
isIdentChar :: Char -> Bool
isIdentChar c = isLetter c || isDigit c || c == '_'

-- Check if an identifier is a keyword
lookupKeyword :: String -> Token
lookupKeyword ident = fromMaybe (TIdent ident) (lookup ident keywords)

scanString :: String -> Position -> LexResult
scanString (_:input) pos = -- We skip the opening quote
  let (content, rest) = span (/= '"') input
  in if null rest
      then (TError "Unterminated string", "", pos)
      else let newPos = advanceBy (length content + 2) pos -- +2 for the quotes
           in (TString content, tail rest, newPos)

scanOperator :: String -> Position -> LexResult
scanOperator (c1:c2:cs) pos
  | [c1,c2] == "==" = (TEq, cs, advanceBy 2 pos)
  | [c1,c2] == "!=" = (TNotEqual, cs, advanceBy 2 pos)
  | [c1,c2] == "<=" = (TLe, cs, advanceBy 2 pos)
  | [c1,c2] == ">=" = (TGe, cs, advanceBy 2 pos)
scanOperator (c:cs) pos
  | c == '+' = (TPlus, cs, advanceBy 1 pos)
  | c == '-' = (TMinus, cs, advanceBy 1 pos)
  | c == '*' = (TMultiply, cs, advanceBy 1 pos)
  | c == '/' = (TDivide, cs, advanceBy 1 pos)
  | c == '%' = (TMod, cs, advanceBy 1 pos)
  | c == '<' = (TLt, cs, advanceBy 1 pos)
  | c == '>' = (TGt, cs, advanceBy 1 pos)
  | c == '=' = (TAssign, cs, advanceBy 1 pos)
scanOperator s pos = (TError ("Invalid operator: " ++ s), s, pos)

scanSymbol :: String -> Position -> LexResult
scanSymbol (c:cs) pos
  | c == '(' = (TLParen, cs, advanceBy 1 pos)
  | c == ')' = (TRParen, cs, advanceBy 1 pos)
  | c == '.' = (TDot, cs, advanceBy 1 pos)
  | c == ',' = (TComma, cs, advanceBy 1 pos)
  | c == ':' = (TColon, cs, advanceBy 1 pos)
scanSymbol s pos = (TError ("Invalid symbol: " ++ s), s, pos)

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
    go [] _ acc =
      TEOF : acc   -- eof

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
                              in go rest pos' (TNewline : acc)
          | isWhitespaceChar c      -> let (rest, pos') = skipWhitespace (c:cs) pos
                              in go rest pos' acc
          | c == '#'       -> let (rest, pos') = skipComment (c:cs) pos
                              in go rest pos' acc
          | otherwise      -> go cs (advancePosition pos) acc

initialPos :: Position
initialPos = Position { line = 1, column = 1 }