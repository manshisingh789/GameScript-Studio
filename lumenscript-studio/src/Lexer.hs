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

scanString (_:input) pos = -- We skip the opening quote
  go input (advanceBy 1 pos) []
  where
    go ('"':rest) endPos content = 
      let payload = TString (reverse content)
          newPos = advanceBy 1 endPos
      in (Token payload pos, rest, newPos)
    go (c:cs) currentPos content = 
      go cs (advanceChar currentPos c) (c:content)
    go "" endPos _ = 
      (Token (TError "Unterminated string") pos, "", endPos)

scanOperator :: String -> Position -> LexResult
scanOperator [] pos = (Token (TError "Unexpected end of file") pos, "", pos)
scanOperator input@(c1:cs) pos =
    case input of
        (c1:c2:rest) -> case lookup [c1,c2] multiCharOps of
                            Just payload -> (Token payload pos, rest, advanceBy 2 pos)
                            Nothing -> single
        _ -> single
  where
    single = case lookup c1 singleCharOps of
               Just payload -> (Token payload pos, cs, advanceBy 1 pos)
               Nothing -> (Token (TError ("Invalid operator: " ++ [c1])) pos, cs, pos)

multiCharOps :: [(String, TokenPayload)]
multiCharOps = [("==", TEq), ("!=", TNotEqual), ("<=", TLe), (">=", TGe)]

singleCharOps :: [(Char, TokenPayload)]
singleCharOps = [('+', TPlus), ('-', TMinus), ('*', TMultiply), ('/', TDivide),
                 ('%', TMod), ('<', TLt), ('>', TGt), ('=', TAssign)]

scanSymbol :: String -> Position -> LexResult
scanSymbol (c:cs) pos =
  case lookup c symbolMap of
    Just payload -> (Token payload pos, cs, advanceBy 1 pos)
    Nothing      -> (Token (TError ("Invalid symbol: " ++ [c])) pos, cs, pos)
scanSymbol "" pos = (Token (TError "Unexpected end of file") pos, "", pos)

symbolMap :: [(Char, TokenPayload)]
symbolMap = [
    ('(', TLParen), (')', TRParen),
    ('{', TBraceL), ('}', TBraceR),
    ('[', TBracketL), (']', TBracketR),
    (',', TComma), ('.', TDot),
    (';', TSemicolon), (':', TColon)
  ]

skipWhitespace :: String -> Position -> (String, Position)
skipWhitespace input pos =
  let (ws, rest) = span isWhitespaceChar input
      newPos     = foldl advanceChar pos ws
  in (rest, newPos)

isWhitespaceChar :: Char -> Bool
isWhitespaceChar c = c `elem` " \t\r"

skipComment :: String -> Position -> (String, Position)
skipComment input pos =
  let (comment, rest) = span (/= '\n') input
      newPos          = advanceBy (length comment) pos
  in (rest, newPos)

lexer :: String -> [Token]
lexer input = reverse $ go input initialPos []
  where
    go :: String -> Position -> [Token] -> [Token]
    go "" pos acc = Token TEOF pos : acc
    go str pos acc =
        let (maybeTok, rest, pos') = lexAtom str pos
        in case maybeTok of
            Just tok -> go rest pos' (tok : acc)
            Nothing  -> go rest pos' acc

type LexAtomResult = (Maybe Token, String, Position)

lexAtom :: String -> Position -> LexAtomResult
lexAtom "" pos = (Just (Token TEOF pos), "", pos)
lexAtom (c:cs) pos
  | isWhitespaceChar c = (Nothing, dropWhile isWhitespaceChar (c:cs), foldl advanceChar pos (takeWhile isWhitespaceChar (c:cs)))
  | c == '\n'          = let (newlines, rest) = span (== '\n') (c:cs)
                             newPos = pos { line = line pos + length newlines, column = 1 }
                         in (Just (Token TNewline pos), rest, newPos)
  | isDigit c          = let (tok, rest, pos') = scanNumber (c:cs) pos in (Just tok, rest, pos')
  | isLetter c         = let (tok, rest, pos') = scanIdentifier (c:cs) pos in (Just tok, rest, pos')
  | c == '"'           = let (tok, rest, pos') = scanString (c:cs) pos in (Just tok, rest, pos')
  | isOperator c       = let (tok, rest, pos') = scanOperator (c:cs) pos in (Just tok, rest, pos')
  | isSymbol c         = let (tok, rest, pos') = scanSymbol (c:cs) pos in (Just tok, rest, pos')
  | c == '#'           = (Nothing, dropWhile (/= '\n') cs, pos { column = column pos + length (takeWhile (/= '\n') cs) + 1 })
  | otherwise          = (Just (Token (TError ("Invalid character: " ++ [c])) pos), cs, advanceBy 1 pos)

advanceChar :: Position -> Char -> Position
advanceChar pos '\n' = pos { line = line pos + 1, column = 1 }
advanceChar pos _    = pos { column = column pos + 1 }


initialPos :: Position
initialPos = Position { line = 1, column = 1 }