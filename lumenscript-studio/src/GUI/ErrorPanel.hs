-- | "Compiler Pipeline & Errors" panel: a stage indicator
-- (Lexing / Parsing / Semantic / Bytecode with a tick, cross or dash) and,
-- for every problem, a card with the line/column, the message, the source
-- line with a caret under the offending text, a "why" and a fix hint.
-- Clicking a card selects the offending text in the editor.
--
-- All the analysis lives in the pure module 'ErrorReport'.
module GUI.ErrorPanel
  ( ErrorPanel(..)
  , createErrorPanel
  , updateErrorPanel
  ) where

import Control.Monad (forM_, void)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import ErrorReport

data ErrorPanel = ErrorPanel
  { xpRoot   :: Element
  , xpStages :: Element
  , xpList   :: Element
  , xpSource :: Element   -- ^ the editor textarea (click a card to select the text)
  }

createErrorPanel :: Element -> UI ErrorPanel
createErrorPanel sourceArea = do
  stages <- UI.div # set UI.style [("margin", "6px 0")]
  list   <- UI.div
  hint   <- UI.div # set UI.style [("font-style", "italic")]
                   # set UI.text "Press Compile to see each stage of the pipeline."
  void $ element list # set children [hint]
  root   <- UI.div #+ [ UI.h2 # set UI.text "Compiler Pipeline & Errors"
                      , element stages
                      , element list ]
  pure (ErrorPanel root stages list sourceArea)

-- | Redraw the stage indicator and the error cards for a fresh compile.
updateErrorPanel :: ErrorPanel -> PipelineReport -> UI ()
updateErrorPanel xp report = do
  chips <- mapM chip (prStages report)
  void $ element (xpStages xp) # set children chips
  cards <- if null (prDiagnostics report)
             then do ok <- UI.div # set UI.text "No errors - every stage passed."
                                  # set UI.style [("color", "#2e7d32"), ("font-weight", "bold")]
                     pure [ok]
             else mapM (card xp (prSource report)) (prDiagnostics report)
  void $ element (xpList xp) # set children cards

stageColour :: StageStatus -> String
stageColour Passed  = "#2e7d32"
stageColour Failed  = "#c62828"
stageColour Skipped = "#757575"

chip :: (Stage, StageStatus) -> UI Element
chip (st, status) =
  UI.span # set UI.text (stageName st ++ " " ++ statusSymbol status)
          # set UI.style
              [ ("display", "inline-block"), ("padding", "2px 10px"), ("margin-right", "8px")
              , ("border-radius", "10px"), ("border", "1px solid " ++ stageColour status)
              , ("color", stageColour status), ("font-weight", "bold"), ("font-family", "monospace") ]

snippetStyle :: SnippetKind -> [(String, String)]
snippetStyle Context   = [("opacity", "0.7")]
snippetStyle ErrorLine = [("font-weight", "bold")]
snippetStyle Caret     = [("color", "#c62828"), ("font-weight", "bold")]

card :: ErrorPanel -> String -> Diagnostic -> UI Element
card xp src d = do
  header <- UI.div # set UI.text (diagnosticHeader d)
                   # set UI.style [("font-weight", "bold"), ("color", "#c62828")]
  msg    <- UI.div # set UI.text (dgMessage d)
  rows   <- mapM (\(k, t) -> UI.div # set UI.text t # set UI.style (snippetStyle k)) (renderSnippet src d)
  snip   <- UI.div # set UI.style
              [ ("font-family", "monospace"), ("white-space", "pre"), ("overflow-x", "auto")
              , ("background", "rgba(128,128,128,0.12)"), ("padding", "6px"), ("margin", "6px 0") ]
            # set children rows
  why    <- UI.div # set UI.text ("Why: " ++ dgWhy d)
  fixIt  <- UI.div # set UI.text ("Fix: " ++ dgHint d) # set UI.style [("color", "#2e7d32")]
  box    <- UI.div # set UI.style
              [ ("border-left", "4px solid #c62828"), ("padding", "6px 10px"), ("margin", "8px 0")
              , ("cursor", "pointer") ]
            #+ map element [header, msg, snip, why, fixIt]
  on UI.click box $ \_ ->
    forM_ (dgOffset d) $ \o ->
      runFunction $ ffi "%1.focus(); %1.setSelectionRange(%2, %3)"
                        (xpSource xp) o (o + dgLength d)
  pure box