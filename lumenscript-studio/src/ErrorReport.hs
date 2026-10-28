-- | Error pipeline + real error visualization.
--
-- Turns what the compiler pipeline produced (tokens, the parser's error,
-- the semantic errors) into:
--
--   * a stage indicator   Lexing ok / Parsing ok / Semantic FAILED / Bytecode skipped
--   * one 'Diagnostic' per problem: line/column, a plain-English message,
--     a "why" explanation and a fix hint
--   * a source snippet with a caret under the exact offending text
--
-- It is pure and GUI-independent.  It deliberately reads errors through
-- their 'show' text (every pipeline error type derives Show), so it does not
-- depend on the error types' constructors and keeps working when new
-- error kinds are added to Parser / Semantic.
module ErrorReport
  ( Stage(..), StageStatus(..)
  , Diagnostic(..)
  , PipelineOutcome(..)
  , PipelineReport(..)
  , SnippetKind(..)
  , buildReport
  , renderSnippet
  , diagnosticHeader
  , stageName, statusSymbol, summaryLine
  , renderPlain
  , humanizeToken
  ) where

import Data.Char (isAlphaNum, isDigit, isSpace, isUpper, toLower)
import Data.List (intercalate, isInfixOf, isPrefixOf, stripPrefix, tails)
import Data.Maybe (fromMaybe, isJust, listToMaybe)

import LinkBuild (tokenLength)
import SourceLink (offsetFromLineCol)
import Token

------------------------------------------------------------------------
-- Types

data Stage = StLex | StParse | StSemantic | StBytecode
  deriving (Eq, Ord, Show, Enum, Bounded)

data StageStatus = Passed | Failed | Skipped
  deriving (Eq, Show)

data Diagnostic = Diagnostic
  { dgStage   :: Stage
  , dgTitle   :: String
  , dgMessage :: String
  , dgWhy     :: String
  , dgHint    :: String
  , dgLine    :: Maybe Int     -- ^ 1-based
  , dgColumn  :: Maybe Int     -- ^ 1-based
  , dgOffset  :: Maybe Int     -- ^ character offset of the pointed-at text
  , dgLength  :: Int           -- ^ how many characters the caret covers
  } deriving (Eq, Show)

-- | Everything the GUI/CLI already has after one 'compileStages' run, with
-- the errors given as their 'show' text.
data PipelineOutcome = PipelineOutcome
  { poSource         :: String
  , poTokens         :: [Token]
  , poParseError     :: Maybe String   -- ^ show of the parser's error, if parsing failed
  , poSemanticErrors :: [String]       -- ^ show of every semantic error
  , poCompiled       :: Bool           -- ^ did bytecode generation succeed?
  }

data PipelineReport = PipelineReport
  { prSource      :: String
  , prStages      :: [(Stage, StageStatus)]
  , prDiagnostics :: [Diagnostic]
  } deriving (Eq, Show)

------------------------------------------------------------------------
-- Report

buildReport :: PipelineOutcome -> PipelineReport
buildReport o = PipelineReport
  { prSource = src
  , prStages = [(StLex, lexSt), (StParse, parseSt), (StSemantic, semSt), (StBytecode, byteSt)]
  , prDiagnostics = lexDiags ++ parseDiags ++ semDiags ++ byteDiags
  }
  where
    src  = poSource o
    toks = poTokens o

    lexErrs  = [ (t, s) | t <- toks, TError s <- [tokenType t] ]
    lexOk    = null lexErrs
    lexSt    = if lexOk then Passed else Failed
    lexDiags = map (lexDiag src toks) lexErrs

    -- A lexing error makes the parser fail too; showing both would be noise,
    -- so parsing is reported as skipped until lexing is clean.
    parseSt
      | not lexOk                  = Skipped
      | isJust (poParseError o)    = Failed
      | otherwise                  = Passed
    parseDiags = case (lexOk, poParseError o) of
      (True, Just s) -> [parseDiag src toks s]
      _              -> []

    semSt
      | parseSt /= Passed              = Skipped
      | not (null (poSemanticErrors o)) = Failed
      | otherwise                      = Passed
    semDiags
      | parseSt /= Passed = []
      | otherwise         = map (semDiag src toks) (poSemanticErrors o)

    byteSt
      | semSt /= Passed = Skipped
      | poCompiled o    = Passed
      | otherwise       = Failed
    byteDiags
      | byteSt == Failed =
          [ Diagnostic StBytecode "Bytecode generation failed"
              "No bytecode was produced." "Semantic analysis passed but the code generator did not return a program."
              "This points at a compiler bug rather than a mistake in your script."
              Nothing Nothing Nothing 1 ]
      | otherwise = []

stageName :: Stage -> String
stageName StLex      = "Lexing"
stageName StParse    = "Parsing"
stageName StSemantic = "Semantic"
stageName StBytecode = "Bytecode"

-- | Check mark / cross / dash.
statusSymbol :: StageStatus -> String
statusSymbol Passed  = "\10003"
statusSymbol Failed  = "\10007"
statusSymbol Skipped = "\8212"

-- | "Lexing ✓  Parsing ✓  Semantic ✗  Bytecode —"
summaryLine :: PipelineReport -> String
summaryLine r = intercalate "  " [ stageName s ++ " " ++ statusSymbol st | (s, st) <- prStages r ]

diagnosticHeader :: Diagnostic -> String
diagnosticHeader d =
  stageName (dgStage d) ++ " error: " ++ dgTitle d ++
  maybe "" (\l -> " (line " ++ show l ++ maybe "" (\c -> ", column " ++ show c) (dgColumn d) ++ ")") (dgLine d)

-- | ASCII-only rendering (safe in a Windows cmd window) used by the terminal
-- demo: stage line, then every diagnostic with its snippet, why and fix.
renderPlain :: PipelineReport -> [String]
renderPlain r =
  [ "Pipeline: " ++ intercalate " | " [ stageName s ++ " " ++ plain st | (s, st) <- prStages r ] ]
  ++ (if null (prDiagnostics r) then ["", "No errors - every stage passed."] else concatMap one (prDiagnostics r))
  where
    plain Passed  = "OK"
    plain Failed  = "FAILED"
    plain Skipped = "--"
    one d =
      [ "", diagnosticHeader d, "  " ++ dgMessage d ]
      ++ map (("  " ++) . snd) (renderSnippet (prSource r) d)
      ++ [ "  Why: " ++ dgWhy d, "  Fix: " ++ dgHint d ]

------------------------------------------------------------------------
-- Locating

-- | line/column -> (line, column, offset, caret length)
locate :: String -> [Token] -> Maybe (Int, Int) -> Maybe Int
       -> (Maybe Int, Maybe Int, Maybe Int, Int)
locate _ _ Nothing _ = (Nothing, Nothing, Nothing, 1)
locate src toks (Just (l, c)) hint = (Just l, Just c, Just off, len)
  where
    off = max 0 (min (length src) (offsetFromLineCol src l c))
    len = fromMaybe (fromMaybe 1 hint) tokLen
    tokLen = listToMaybe
      [ max 1 (tokenLength t)
      | t <- toks, let Position tl tc = tokenPosition t, tl == l, tc == c ]

-- | Pull "line = N" / "column = M" out of a show string.
findPos :: String -> Maybe (Int, Int)
findPos s = do
  l <- numAfter "line = " s
  c <- numAfter "column = " s
  pure (l, c)

numAfter :: String -> String -> Maybe Int
numAfter key s =
  case [ drop (length key) t | t <- tails s, key `isPrefixOf` t ] of
    (rest : _) -> case span isDigit rest of
                    ([], _)  -> Nothing
                    (ds, _)  -> Just (read ds)
    []         -> Nothing

------------------------------------------------------------------------
-- Lexing errors

lexDiag :: String -> [Token] -> (Token, String) -> Diagnostic
lexDiag src toks (t, s) = Diagnostic
  { dgStage = StLex
  , dgTitle = "unrecognized text"
  , dgMessage = if null s then "Unexpected character" else "Unexpected `" ++ s ++ "`"
  , dgWhy = if take 1 s == "\""
              then "A string was opened with a quote but never closed."
              else "The lexer does not recognize this text as part of the LumenScript language, so it cannot turn it into a token."
  , dgHint = if take 1 s == "\""
               then "Add the closing quote."
               else "Remove it, or replace it with a valid symbol, name or number."
  , dgLine = l, dgColumn = c, dgOffset = off, dgLength = len
  }
  where
    Position pl pc = tokenPosition t
    (l, c, off, len) = locate src toks (Just (pl, pc)) (Just (length s))

------------------------------------------------------------------------
-- Parse errors

parseDiag :: String -> [Token] -> String -> Diagnostic
parseDiag src toks s = Diagnostic
  { dgStage = StParse
  , dgTitle = "syntax"
  , dgMessage = message, dgWhy = why, dgHint = hint
  , dgLine = l, dgColumn = c, dgOffset = off, dgLength = len }
  where
    raw = case reads (dropWhile (/= '"') s) :: [(String, String)] of
            [(m, _)] -> m
            _        -> s
    (message, why, hint) = explainParse raw
    (l, c, off, len) = locate src toks (findPos s) Nothing

explainParse :: String -> (String, String, String)
explainParse raw = case expectedGot raw of
  Just (e, g) ->
    ( "Expected " ++ humanizeToken e ++ " but found " ++ humanizeToken g
    , "The parser was in the middle of a statement and needed " ++ humanizeToken e
        ++ " next, but the next token was " ++ humanizeToken g ++ "."
    , parseHint (headWord e) (headWord g) )
  Nothing ->
    ( raw
    , "The parser found a sequence of tokens that does not match any LumenScript statement or expression here."
    , "Compare this line with the scripts in the demos folder." )
  where headWord = takeWhile (not . isSpace)

expectedGot :: String -> Maybe (String, String)
expectedGot m = do
  rest <- stripPrefix "Expected " m
  splitOnSub " but got " rest

parseHint :: String -> String -> String
parseHint e g = case (e, g) of
  ("TColon", "TBraceL") -> "A LumenScript block is opened with `:` before `{` - for example `if x > 1: {`, `else: {` or `on key_press SPACE: {`."
  ("TColon", _)         -> "Add a `:` here (blocks look like `on key_press SPACE: {`)."
  ("TBraceL", _)        -> "Open the block with `{`."
  ("TBraceR", "TEOF")   -> "The file ended before every `{` was closed - add the missing `}`."
  ("TBraceR", _)        -> "Close the block with `}`."
  ("TRParen", _)        -> "Add the missing `)` - every `(` needs a matching `)`."
  ("TLParen", _)        -> "Add the missing `(`."
  ("TAssign", _)        -> "A declaration needs `=` followed by a value, for example `let x = 5`."
  ("TIdent", _)         -> "A name (identifier) is required here."
  (_, "TEOF")           -> "The file ended unexpectedly - something is missing at the end."
  (_, "TNewline")       -> "The line ended too early - the statement is incomplete."
  _                     -> "Check the syntax around this position."

-- | "TColon" -> "`:`", "TIdent \"x\"" -> "name \"x\"", ...
humanizeToken :: String -> String
humanizeToken t = case words t of
  []         -> t
  (w : rest) -> let base = fromMaybe w (lookup w tokenNames)
                in unwords (base : rest)

tokenNames :: [(String, String)]
tokenNames =
  [ ("TColon", "`:`"), ("TBraceL", "`{`"), ("TBraceR", "`}`"), ("TLParen", "`(`"), ("TRParen", "`)`")
  , ("TBracketL", "`[`"), ("TBracketR", "`]`"), ("TSemicolon", "`;`"), ("TComma", "`,`"), ("TDot", "`.`")
  , ("TAssign", "`=`"), ("TPlus", "`+`"), ("TMinus", "`-`"), ("TMultiply", "`*`"), ("TDivide", "`/`")
  , ("TMod", "`%`"), ("TLt", "`<`"), ("TGt", "`>`"), ("TEq", "`==`"), ("TNotEqual", "`!=`")
  , ("TLe", "`<=`"), ("TGe", "`>=`"), ("TNot", "`!`")
  , ("TNewline", "end of line"), ("TBlank", "blank"), ("TEOF", "end of file")
  , ("TKwLet", "`let`"), ("TKwConst", "`const`"), ("TKwIf", "`if`"), ("TKwElse", "`else`")
  , ("TKwElif", "`elif`"), ("TKwOn", "`on`"), ("TKwKeyPress", "`key_press`")
  , ("TKwCollision", "`collision`"), ("TKwUpdate", "`update`"), ("TKwInteract", "`interact`")
  , ("TIdent", "name"), ("TInt", "number"), ("TString", "string"), ("TBool", "boolean") ]

splitOnSub :: String -> String -> Maybe (String, String)
splitOnSub pat = go []
  where
    go acc r | pat `isPrefixOf` r = Just (reverse acc, drop (length pat) r)
    go _ []       = Nothing
    go acc (x : xs) = go (x : acc) xs

------------------------------------------------------------------------
-- Semantic errors

semDiag :: String -> [Token] -> String -> Diagnostic
semDiag src toks s = Diagnostic
  { dgStage = StSemantic
  , dgTitle = prettyCtor ctor
  , dgMessage = message, dgWhy = why, dgHint = hint
  , dgLine = l, dgColumn = c, dgOffset = off, dgLength = len }
  where
    ctor   = takeWhile (\ch -> isAlphaNum ch || ch == '_') s
    fields = parseFields s
    name   = lookup "errName" fields
    (message, why, hint) = explainSemantic ctor fields
    (l, c, off, len) = locate src toks (findPos s) (fmap length name)

explainSemantic :: String -> [(String, String)] -> (String, String, String)
explainSemantic ctor fields
  | has "member" =
      ( "`" ++ base ++ "." ++ n ++ "` does not exist"
      , "`" ++ base ++ "` is defined, but it has no member called `" ++ n ++ "`."
      , "Check the spelling, or look in HostBindings.hs for the members `" ++ base ++ "` really has." )
  | has "undefined" || has "unbound" || has "notdeclared" =
      ( "`" ++ n ++ "` is not defined"
      , "`" ++ n ++ "` is used here, but no declaration of it is visible in this scope."
      , "Declare it first (`let " ++ n ++ " = ...`) or fix the spelling." )
  | has "const" || has "immutable" =
      ( "`" ++ n ++ "` cannot be changed"
      , "`" ++ n ++ "` was declared with `const`, so it cannot be reassigned."
      , "Declare it with `let` instead if it needs to change." )
  | has "redecl" || has "duplicate" || has "already" =
      ( "`" ++ n ++ "` is declared more than once"
      , "A name can only be declared once in the same scope."
      , "Rename one of them or remove the duplicate." )
  | has "type" || has "mismatch" =
      ( prettyCtor ctor ++ detail
      , "The types of the values involved are not compatible with each other."
      , "Check that both sides of the operation have the same kind of value (number, string or boolean)." )
  | has "arg" || has "arity" =
      ( prettyCtor ctor ++ detail
      , "The call passes a different number of arguments than the function expects."
      , "Check how many arguments this call takes." )
  | otherwise =
      ( prettyCtor ctor ++ detail
      , "The semantic analyzer checks that names are declared, used correctly and have consistent types; this rule was broken."
      , "Look at the highlighted position and compare it with the rule named above." )
  where
    lc     = map toLower ctor
    has k  = k `isInfixOf` lc
    base   = fromMaybe "?" (lookup "errBase" fields)
    n      = fromMaybe "?" (lookup "errName" fields)
    shown  = [ k ++ " = " ++ v | (k, v) <- fields, k /= "errPos" ]
    detail = if null shown then "" else " (" ++ intercalate ", " shown ++ ")"

-- "UndefinedMember" -> "undefined member"
prettyCtor :: String -> String
prettyCtor [] = []
prettyCtor (x : xs) = toLower x : concatMap (\ch -> if isUpper ch then [' ', toLower ch] else [ch]) xs

-- | Parse the `{ key = value, ... }` part of a derived Show record.
parseFields :: String -> [(String, String)]
parseFields s = case dropWhile (/= '{') s of
  []         -> []
  (_ : rest) -> go rest
  where
    go r0 =
      let r = dropWhile (\ch -> isSpace ch || ch == ',') r0
      in case r of
           []        -> []
           ('}' : _) -> []
           _ ->
             let (key, afterKey) = span (\ch -> isAlphaNum ch || ch == '_') r
                 afterEq = dropWhile (\ch -> isSpace ch || ch == '=') afterKey
             in if null key
                  then []
                  else case afterEq of
                    ('"' : _) -> case reads afterEq :: [(String, String)] of
                                   [(v, rest)] -> (key, v) : go rest
                                   _           -> []
                    _ -> let (v, rest) = takeBalanced afterEq in (key, v) : go rest

takeBalanced :: String -> (String, String)
takeBalanced = go (0 :: Int) []
  where
    go _ acc [] = (reverse acc, [])
    go d acc (ch : cs)
      | ch == '{'            = go (d + 1) (ch : acc) cs
      | ch == '}' && d == 0  = (reverse acc, ch : cs)
      | ch == '}'            = go (d - 1) (ch : acc) cs
      | ch == ',' && d == 0  = (reverse acc, ch : cs)
      | otherwise            = go d (ch : acc) cs

------------------------------------------------------------------------
-- Snippet with caret

data SnippetKind = Context | ErrorLine | Caret
  deriving (Eq, Show)

-- | The offending line (plus one line of context each side) and a caret
-- line pointing at the exact column:
--
-- >   3 |   on key_press SPACE: {
-- > > 4 |   if score > 5 {
-- >     |                ^
renderSnippet :: String -> Diagnostic -> [(SnippetKind, String)]
renderSnippet src d = case (dgLine d, dgColumn d) of
  (Just l0, Just c0) | not (null ls) ->
    let l       = max 1 (min total l0)
        content = ls !! (l - 1)
        c       = if l0 > total then length content + 1 else c0
        before  = [ (Context, "  " ++ gutter i ++ " | " ++ expand (ls !! (i - 1))) | i <- [l - 1], i >= 1 ]
        after   = [ (Context, "  " ++ gutter i ++ " | " ++ expand (ls !! (i - 1))) | i <- [l + 1], i <= total ]
        pad     = length (concatMap expandChar (take (c - 1) content))
        errLine = (ErrorLine, "> " ++ gutter l ++ " | " ++ expand content)
        caret   = (Caret, "  " ++ replicate width ' ' ++ " | " ++ replicate pad ' ' ++ replicate (max 1 (dgLength d)) '^')
    in before ++ [errLine, caret] ++ after
  _ -> []
  where
    ls    = lines src
    total = length ls
    width = length (show total)
    gutter i = let t = show i in replicate (width - length t) ' ' ++ t
    expand = concatMap expandChar
    expandChar '\t' = "    "
    expandChar ch   = [ch]