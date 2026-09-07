module GUI.Panel
  ( formatAsLines
  , placeholderIfEmpty
  , createListPanel
  , updateListPanel
  ) where

import Control.Monad (void)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- | Shared plumbing for the four "list of lines" viewer panels
-- (Token/AST/Bytecode/StackTrace). Each of those modules is a thin,
-- specifically-named wrapper around the functions here -- this is
-- where the actual DOM-building and text-formatting logic lives, so
-- it's written (and tested) once instead of four times.

-- | Turn anything Show-able into one line of text per line of its
-- `show` output. Works for real data once a panel is wired to it
-- (e.g. a real CompiledProgram), regardless of whether that Show
-- instance happens to be single-line or multi-line.
formatAsLines :: Show a => a -> [String]
formatAsLines = lines . show

-- | Swap in a friendly placeholder message when there's nothing to
-- show yet, rather than rendering an empty panel.
placeholderIfEmpty :: String -> [String] -> [String]
placeholderIfEmpty msg [] = [msg]
placeholderIfEmpty _   xs = xs

-- | Build a titled panel: an <h2> heading plus a monospace content
-- area pre-filled with `initialLines` (or `placeholderMsg` if empty).
-- Returns both the panel's root (attach this to the page) and its
-- content element (hand this to 'updateListPanel' later to refresh
-- it).
createListPanel :: String -> String -> [String] -> UI (Element, Element)
createListPanel title placeholderMsg initialLines = do
  content <- UI.div
    # set UI.style
        [ ("font-family", "monospace")
        , ("white-space", "pre")
        , ("font-size", "13px")
        , ("border", "1px solid #444")
        , ("padding", "6px")
        , ("min-height", "80px")
        , ("overflow-y", "auto")
        ]
  root <- UI.div
    #+ [ UI.h2 # set UI.text title
       , element content
       ]
  updateListPanel content placeholderMsg initialLines
  pure (root, content)

-- | Replace a panel's content with one row per line, or the
-- placeholder message if `newLines` is empty.
updateListPanel :: Element -> String -> [String] -> UI ()
updateListPanel content placeholderMsg newLines = do
  rows <- mapM (\line -> UI.div # set UI.text line) (placeholderIfEmpty placeholderMsg newLines)
  void $ element content # set children rows