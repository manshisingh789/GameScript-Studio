-- | "Pipeline Link" panel: Source / Tokens / AST / Bytecode side by side.
-- Click a row (or a token in the Source column, or move the caret in the
-- editor) and the matching items light up in every column.
--
-- All linking logic lives in the pure modules 'SourceLink' and 'LinkBuild';
-- this module only builds DOM rows and repaints them.
module GUI.LinkPanel
  ( LinkPanel(..)
  , createLinkPanel
  , updateLinkPanel
  , selectInLinkPanel
  , syncFromEditor
  ) where

import Control.Monad (foldM, forM_, void, when)
import Control.Monad.IO.Class (liftIO)
import Data.IORef
import qualified Data.IntSet as IS
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import SourceLink

-- | Everything needed to repaint after a selection (rebuilt on each Compile).
data LinkState = LinkState
  { lsTable     :: LinkTable
  , lsTokenRows :: [Element]
  , lsAstRows   :: [Element]
  , lsCodeRows  :: [Element]
  , lsSourceEls :: [(Int, Element)]   -- ^ token index, its span in the Source column
  }

data LinkPanel = LinkPanel
  { lpRoot   :: Element
  , lpInfo   :: Element
  , lpSource :: Element
  , lpTokens :: Element
  , lpAst    :: Element
  , lpCode   :: Element
  , lpState  :: IORef (Maybe LinkState)
  }

boxStyle :: [(String, String)]
boxStyle =
  [ ("font-family", "monospace"), ("white-space", "pre"), ("font-size", "13px")
  , ("border", "1px solid #444"), ("padding", "6px"), ("min-height", "80px")
  , ("max-height", "320px"), ("overflow", "auto") ]

rowStyle :: Bool -> [(String, String)]
rowStyle hot =
  [ ("cursor", "pointer"), ("white-space", "pre")
  , ("background", if hot then "#ffd54f" else "transparent")
  , ("color", if hot then "#000" else "inherit") ]

srcStyle :: Bool -> [(String, String)]
srcStyle hot =
  [ ("cursor", "pointer")
  , ("background", if hot then "#ffd54f" else "transparent")
  , ("color", if hot then "#000" else "inherit") ]

createLinkPanel :: UI LinkPanel
createLinkPanel = do
  info <- UI.div # set UI.style [("font-style", "italic")]
                 # set UI.text "Compile, then click a token, AST node or instruction."
  srcBox <- UI.div # set UI.style boxStyle
  tokBox <- UI.div # set UI.style boxStyle
  astBox <- UI.div # set UI.style boxStyle
  codeBox <- UI.div # set UI.style boxStyle
  let column title box = UI.div # set UI.style [("flex", "1"), ("min-width", "0")]
        #+ [ UI.div # set UI.style [("font-weight", "bold")] # set UI.text title
           , element box ]
  row <- UI.div # set UI.style [("display", "flex"), ("gap", "8px")]
    #+ [ column "Source" srcBox, column "Tokens" tokBox
       , column "AST" astBox, column "Bytecode" codeBox ]
  root <- UI.div #+ [ UI.h2 # set UI.text "Pipeline Link", element info, element row ]
  ref <- liftIO (newIORef Nothing)
  pure (LinkPanel root info srcBox tokBox astBox codeBox ref)

-- | Rebuild all four columns for a freshly compiled program.
updateLinkPanel :: LinkPanel -> LinkTable -> UI ()
updateLinkPanel lp table = do
  tokRows  <- mapM (mkRow lp SelToken) (zip [0 ..] (ltTokens table))
  astRows  <- mapM (mkRow lp SelAst)   (zip [0 ..] (ltAst table))
  codeRows <- mapM (mkRow lp SelCode)  (zip [0 ..] (ltCode table))
  srcEls   <- buildSource lp table
  codeShown <- if null codeRows
    then do p <- UI.div # set UI.text "(no bytecode -- program did not compile)"
            pure [p]
    else pure codeRows
  void $ element (lpTokens lp) # set children tokRows
  void $ element (lpAst lp)    # set children astRows
  void $ element (lpCode lp)   # set children codeShown
  void $ element (lpSource lp) # set children (map snd srcEls)
  liftIO $ writeIORef (lpState lp) (Just (LinkState table tokRows astRows codeRows [ (i, e) | (Just i, e) <- srcEls ]))
  void $ element (lpInfo lp) # set UI.text
    (if null (ltNote table)
       then "Click a token, AST node or instruction (or move the editor caret)."
       else ltNote table)
  paint lp emptyHighlights

mkRow :: LinkPanel -> (Int -> Selection) -> (Int, Item) -> UI Element
mkRow lp mk (i, it) = do
  r <- UI.div # set UI.text (itLabel it) # set UI.style (rowStyle False)
  on UI.click r $ \_ -> selectInLinkPanel lp (mk i)
  pure r

-- | The Source column: the program text cut at token boundaries, every
-- token clickable. Gaps (spaces) are plain text.
buildSource :: LinkPanel -> LinkTable -> UI [(Maybe Int, Element)]
buildSource lp table = do
  let src  = ltSource table
      slice a b = take (b - a) (drop a src)
      toks = [ (i, s) | (i, Item _ (Just s)) <- zip [0 :: Int ..] (ltTokens table), not (isEmptySpan s) ]
      go (cur, acc) (i, Span a b)
        | a < cur   = pure (cur, acc)
        | otherwise = do
            gap <- if a > cur
                     then do g <- UI.span # set UI.text (slice cur a); pure [(Nothing, g)]
                     else pure []
            t <- UI.span # set UI.text (slice a b) # set UI.style (srcStyle False)
            on UI.click t $ \_ -> selectInLinkPanel lp (SelToken i)
            pure (b, acc ++ gap ++ [(Just i, t)])
  (end, pieces) <- foldM go (0, []) toks
  tailEl <- if end < length src
              then do t <- UI.span # set UI.text (drop end src); pure [(Nothing, t)]
              else pure []
  pure (pieces ++ tailEl)

-- | Highlight everything linked to the selection, in all four columns.
selectInLinkPanel :: LinkPanel -> Selection -> UI ()
selectInLinkPanel lp sel = do
  ms <- liftIO (readIORef (lpState lp))
  forM_ ms $ \ls -> do
    let hl = resolve (lsTable ls) sel
    paint lp hl
    void $ element (lpInfo lp) # set UI.text
      ( (if null (ltNote (lsTable ls)) then "" else ltNote (lsTable ls) ++ "  |  ")
     ++ "linked -> tokens: " ++ show (IS.size (hlTokens hl))
     ++ "  ast nodes: "      ++ show (IS.size (hlAst hl))
     ++ "  instructions: "   ++ show (IS.size (hlCode hl)) )

paint :: LinkPanel -> Highlights -> UI ()
paint lp hl = do
  ms <- liftIO (readIORef (lpState lp))
  forM_ ms $ \ls -> do
    colour (lsTokenRows ls) (hlTokens hl)
    colour (lsAstRows ls)   (hlAst hl)
    colour (lsCodeRows ls)  (hlCode hl)
    forM_ (lsSourceEls ls) $ \(i, e) ->
      void $ element e # set UI.style (srcStyle (IS.member i (hlTokens hl)))
  where
    colour rows selected = do
      forM_ (zip [0 ..] rows) $ \(i, e) ->
        void $ element e # set UI.style (rowStyle (IS.member i selected))
      -- keep the first highlighted row visible inside its scrolling column
      case [ e | (i, e) <- zip [0 ..] rows, IS.member i selected ] of
        (e : _) -> runFunction $ ffi "%1.scrollIntoView({block: 'nearest'})" e
        []      -> pure ()

callInt :: JSFunction Int -> UI Int
callInt = callFunction

-- | Editor caret / selection -> highlights. Ignored if the text was edited
-- since the last Compile (offsets would no longer match the table).
syncFromEditor :: LinkPanel -> Element -> UI ()
syncFromEditor lp area = do
  ms <- liftIO (readIORef (lpState lp))
  forM_ ms $ \ls -> do
    current <- get UI.value area
    when (current == ltSource (lsTable ls)) $ do
      a <- callInt (ffi "%1.selectionStart" area)
      b <- callInt (ffi "%1.selectionEnd" area)
      selectInLinkPanel lp (SelSource (Span (min a b) (max a b)))