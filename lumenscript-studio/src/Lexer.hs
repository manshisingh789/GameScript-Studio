module Lexer where

import Data.Char (isDigit, isLetter, isSpace)
import Token

-- Position tracking
data Position = Position { line :: Int, column :: Int }
  deriving (Show, Eq)

type LexResult = (Token, String, Position)

-- Empty helper functions
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

scanIdentifier :: String -> Position -> LexResult
scanIdentifier input pos = undefined

scanString :: String -> Position -> LexResult
scanString input pos = undefined

scanOperator :: String -> Position -> LexResult
scanOperator input pos = undefined

scanSymbol :: String -> Position -> LexResult
scanSymbol input pos = undefined

skipWhitespace :: String -> Position -> (String, Position)
skipWhitespace input pos = undefined

skipComment :: String -> Position -> (String, Position)
skipComment input pos = undefined

emitNewline :: Position -> Position
emitNewline pos = undefined

advancePosition :: Position -> Position
advancePosition pos = undefined

-- 🔹 Scanner loop outline (add this after helpers)
lexer :: String -> [Token]
lexer input = go input initialPos []
  where
    go :: String -> Position -> [Token] -> [Token]
    go [] pos acc =
      acc ++ [TEOF]   -- eof

    go (c:cs) pos acc =
      case c of
        _ | isDigit c      -> let (tok, rest, pos') = scanNumber (c:cs) pos
                              in go rest pos' (acc ++ [tok])
          | isLetter c     -> let (tok, rest, pos') = scanIdentifier (c:cs) pos
                              in go rest pos' (acc ++ [tok])
          | c == '"'       -> let (tok, rest, pos') = scanString (c:cs) pos
                              in go rest pos' (acc ++ [tok])
          | isOperator c   -> let (tok, rest, pos') = scanOperator (c:cs) pos
                              in go rest pos' (acc ++ [tok])
          | isSymbol c     -> let (tok, rest, pos') = scanSymbol (c:cs) pos
                              in go rest pos' (acc ++ [tok])
          | isSpace c      -> let (rest, pos') = skipWhitespace (c:cs) pos
                              in go rest pos' acc
          | c == '\n'      -> let pos' = emitNewline pos
                              in go cs pos' acc
          | c == '/'       -> let (rest, pos') = skipComment (c:cs) pos
                              in go rest pos' acc
          | otherwise      -> go cs (advancePosition pos) acc

initialPos :: Position
initialPos = Position { line = 1, column = 1 }