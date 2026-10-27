-- | Source -> Token -> AST -> Bytecode synchronized highlighting.
--
-- Pure, GUI-independent engine. Every stage item carries an optional source
-- 'Span' (half-open character offsets into the source text).  A 'Selection'
-- in ANY panel is turned into a source span (the "pivot"); the pivot is then
-- mapped forward/backward to the other three stages.
module SourceLink
  ( Span(..), spanLen, isEmptySpan, containsSpan, overlapsSpan, hullSpans
  , Item(..)
  , LinkTable(..)
  , Selection(..)
  , Highlights(..), emptyHighlights
  , resolve
  , segments
  , renderReport
  , offsetFromLineCol
  ) where

import qualified Data.IntSet as IS
import Data.List (sortOn)

-- | Half-open character range [spStart, spEnd) into the source text.
data Span = Span { spStart :: !Int, spEnd :: !Int }
  deriving (Eq, Ord, Show)

spanLen :: Span -> Int
spanLen (Span a b) = b - a

isEmptySpan :: Span -> Bool
isEmptySpan s = spanLen s <= 0

-- | @containsSpan outer inner@
containsSpan :: Span -> Span -> Bool
containsSpan (Span a b) (Span c d) = a <= c && d <= b

overlapsSpan :: Span -> Span -> Bool
overlapsSpan (Span a b) (Span c d) = a < d && c < b

hullSpans :: [Span] -> Maybe Span
hullSpans [] = Nothing
hullSpans ss = Just (Span (minimum (map spStart ss)) (maximum (map spEnd ss)))

-- | One entry of a stage panel (a token, an AST node or an instruction).
data Item = Item
  { itLabel :: String
  , itSpan  :: Maybe Span   -- ^ Nothing for synthetic items (e.g. HALT)
  } deriving (Eq, Show)

-- | The four stages. The list position of an 'Item' is its id.
data LinkTable = LinkTable
  { ltSource :: String
  , ltTokens :: [Item]
  , ltAst    :: [Item]
  , ltCode   :: [Item]
  , ltNote   :: String   -- ^ non-empty = a warning to show the user
  } deriving (Eq, Show)

-- | What the user clicked / selected, in any panel.
data Selection
  = SelSource Span     -- ^ cursor (empty span) or selected range in the editor
  | SelToken Int
  | SelAst Int
  | SelCode Int
  deriving (Eq, Show)

-- | What every panel should draw as highlighted.
data Highlights = Highlights
  { hlSource :: [Span]
  , hlTokens :: IS.IntSet
  , hlAst    :: IS.IntSet
  , hlCode   :: IS.IntSet
  } deriving (Eq, Show)

emptyHighlights :: Highlights
emptyHighlights = Highlights [] IS.empty IS.empty IS.empty

indexed :: [Item] -> [(Int, Span)]
indexed items = [ (i, s) | (i, Item _ (Just s)) <- zip [0 ..] items, not (isEmptySpan s) ]

at :: Int -> [a] -> Maybe a
at i xs | i < 0 = Nothing
        | otherwise = case drop i xs of (x:_) -> Just x; [] -> Nothing

-- Tokens touched by a non-empty pivot.
tokensOverlapping :: Span -> [Item] -> IS.IntSet
tokensOverlapping p ts = IS.fromList [ i | (i, s) <- indexed ts, overlapsSpan p s ]

-- Token(s) under a cursor offset (token containing it; else one ending at it).
tokensAtOffset :: Int -> [Item] -> IS.IntSet
tokensAtOffset q ts =
  case [ i | (i, Span a b) <- indexed ts, a <= q, q < b ] of
    [] -> IS.fromList [ i | (i, Span _ b) <- indexed ts, b == q ]
    xs -> IS.fromList xs

-- Items fully inside the pivot; if none, the smallest items covering it.
insideElseCover :: Span -> [Item] -> IS.IntSet
insideElseCover p items
  | not (null inside) = IS.fromList (map fst inside)
  | otherwise         = minimalCover p items
  where inside = [ x | x@(_, s) <- indexed items, containsSpan p s ]

-- Smallest items whose span covers the pivot (ties all kept).
minimalCover :: Span -> [Item] -> IS.IntSet
minimalCover p items = case covers of
    [] -> IS.empty
    _  -> let w = minimum (map (spanLen . snd) covers)
          in IS.fromList [ i | (i, s) <- covers, spanLen s == w ]
  where covers = [ x | x@(_, s) <- indexed items, containsSpan s p ]

-- | Map a selection in any panel to highlights in all four.
resolve :: LinkTable -> Selection -> Highlights
resolve lt sel = case sel of
  SelSource sp -> case normalize sp of
    Nothing -> emptyHighlights
    Just p  -> fromPivot p

  SelToken i -> case at i (ltTokens lt) >>= itSpan of
    Nothing -> emptyHighlights
    Just p  -> (fromPivot p) { hlTokens = IS.singleton i }

  SelAst i -> case at i (ltAst lt) >>= itSpan of
    Nothing -> emptyHighlights { hlAst = IS.singleton i }
    Just p  -> Highlights
      { hlSource = [p]
      , hlTokens = tokensOverlapping p (ltTokens lt)
      , hlAst    = IS.singleton i
      , hlCode   = insideElseCover p (ltCode lt) }

  SelCode i -> case at i (ltCode lt) >>= itSpan of
    Nothing -> emptyHighlights { hlCode = IS.singleton i }   -- synthetic instr.
    Just p  -> Highlights
      { hlSource = [p]
      , hlTokens = tokensOverlapping p (ltTokens lt)
      , hlAst    = minimalCover p (ltAst lt)
      , hlCode   = IS.singleton i }
  where
    fromPivot p = Highlights
      { hlSource = [p]
      , hlTokens = tokensOverlapping p (ltTokens lt)
      , hlAst    = insideElseCover p (ltAst lt)
      , hlCode   = insideElseCover p (ltCode lt) }

    -- An empty span (plain cursor) becomes the hull of the token(s) under it.
    normalize sp
      | not (isEmptySpan sp) = Just sp
      | otherwise = hullSpans
          [ s | i <- IS.toList (tokensAtOffset (spStart sp) (ltTokens lt))
              , Just s <- [at i (ltTokens lt) >>= itSpan] ]

-- | Split source text into (isHighlighted, text) runs, for any GUI toolkit.
segments :: String -> [Span] -> [(Bool, String)]
segments src spans = go 0 merged
  where
    n = length src
    clamp (Span a b) = Span (max 0 a) (min n b)
    merged = mergeSpans (map clamp spans)
    go pos [] = [ (False, drop pos src) | pos < n ]
    go pos (Span a b : rest) =
         [ (False, take (a - pos) (drop pos src)) | a > pos ]
      ++ [ (True,  take (b - a)   (drop a src)) ]
      ++ go b rest

mergeSpans :: [Span] -> [Span]
mergeSpans = go . sortOn spStart . filter (not . isEmptySpan)
  where
    go (a : b : rest)
      | spStart b <= spEnd a = go (Span (spStart a) (max (spEnd a) (spEnd b)) : rest)
      | otherwise            = a : go (b : rest)
    go xs = xs

-- | Plain-text rendering of all four panels (ASCII only, safe for Windows cmd).
renderReport :: LinkTable -> Highlights -> String
renderReport lt hl = unlines $
     [ "== SOURCE ( [[ ]] = highlighted ) ==" ]
  ++ lines (concatMap mark (segments (ltSource lt) (hlSource hl)))
  ++ [ "", "== TOKENS ==" ]  ++ panel (ltTokens lt) (hlTokens hl)
  ++ [ "", "== AST =="    ]  ++ panel (ltAst lt)    (hlAst hl)
  ++ [ "", "== BYTECODE ==" ] ++ panel (ltCode lt)   (hlCode hl)
  where
    mark (True,  t) = "[[" ++ t ++ "]]"
    mark (False, t) = t
    panel items sel =
      [ (if IS.member i sel then ">> " else "   ") ++ pad (show i) ++ " " ++ label it
      | (i, it) <- zip [0 :: Int ..] items ]
    pad s = replicate (3 - length s) ' ' ++ s
    label (Item l sp) = l ++ maybe "" (\(Span a b) -> "  @" ++ show a ++ "-" ++ show b) sp

-- | 1-based line/column (as shown in an editor) -> character offset.
offsetFromLineCol :: String -> Int -> Int -> Int
offsetFromLineCol src line col = lineStart + (col - 1)
  where lineStart = sum (map ((+ 1) . length) (take (line - 1) (lines src)))