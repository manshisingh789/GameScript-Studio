module ParserTypes where

import Token

data ParseError = ParseError String Position
  deriving (Show, Eq)

-- Every parse function: consume some tokens, return the parsed value
-- plus whatever tokens are left over.
type ParseResult a = Either ParseError (a, [Token])

-- Position to blame when we run out of tokens entirely
eofPosition :: Position
eofPosition = Position (-1) (-1)

peek :: [Token] -> Maybe Token
peek []    = Nothing
peek (t:_) = Just t

-- Consume one token of the expected type, or error
expect :: TokenPayload -> [Token] -> Either ParseError [Token]
expect expected (t:ts)
  | tokenType t == expected = Right ts
  | otherwise = Left (ParseError
      ("Expected " ++ show expected ++ " but got " ++ show (tokenType t))
      (tokenPosition t))
expect expected [] = Left (ParseError
  ("Expected " ++ show expected ++ " but reached end of input")
  eofPosition)

-- The lexer emits TNewline/TBlank tokens; statement-level parsing
-- (Person B's side) needs to skip these between statements.
skipNewlines :: [Token] -> [Token]
skipNewlines (t:ts)
  | tokenType t == TNewline || tokenType t == TBlank = skipNewlines ts
skipNewlines ts = ts