module GUI.EditorPanel
  ( EditorPanel(..)
  , createEditorPanel
  ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

-- | The script editor panel: a textarea for LumenScript source plus a
-- "Compile" button and a status line for compile results. This module
-- only builds the widget tree and hands back handles to it -- it
-- knows nothing about Lexer/Parser/Compiler, so it stays swappable
-- and testable on its own. The wiring (what "Compile" actually does)
-- lives in GUI.App.
data EditorPanel = EditorPanel
  { epRoot          :: Element  -- ^ the panel's container, ready to attach to the page
  , epSourceArea    :: Element  -- ^ the textarea itself, for reading .value out of
  , epCompileButton :: Element  -- ^ the "Compile" button, for wiring an `on UI.click`
  , epStatus        :: Element  -- ^ a status line ("Compiled OK" / error text)
  }

-- | Build the editor panel, pre-filled with `initialSource` (e.g. a
-- demo script) so the panel is never shown empty.
createEditorPanel :: String -> UI EditorPanel
createEditorPanel initialSource = do
  sourceArea <- UI.textarea
    # set UI.value initialSource
    # set (UI.attr "spellcheck") "false"
    # set UI.style
        [ ("width", "100%")
        , ("height", "300px")
        , ("font-family", "monospace")
        , ("font-size", "14px")
        ]

  compileButton <- UI.button
    # set UI.text "Compile"
    # set UI.style [("margin-top", "8px")]

  status <- UI.p
    # set UI.text ""
    # set UI.style [("font-style", "italic"), ("white-space", "pre-wrap")]

  root <- UI.div
    #+ [ UI.h2 # set UI.text "Editor"
       , element sourceArea
       , element compileButton
       , element status
       ]

  pure EditorPanel
    { epRoot          = root
    , epSourceArea    = sourceArea
    , epCompileButton = compileButton
    , epStatus        = status
    }