module GUI.Panel
  ( formatAsLines
  , placeholderIfEmpty
  , createListPanel
  , updateListPanel
  , createTokenPanel
  , updateTokenPanel
  ) where

import Control.Monad (void, forM)
import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core hiding (column)
import Token (Token(..), TokenPayload(..), Position(..))
import Data.Char (isControl)

-- | Helper to get the length of a token's lexeme.
-- This is used to calculate the selection range in the editor.
getTokenLength :: Token -> Int
getTokenLength token = case tokenType token of
  TInt n -> length (show n)
  TString s -> length s + 2 -- Add 2 for the quotes
  TBool b -> length (show b)
  TIdent s -> length s
  TDot -> 1
  TComma -> 1
  TColon -> 1
  TSemicolon -> 1
  TLParen -> 1
  TRParen -> 1
  TBraceL -> 1
  TBraceR -> 1
  TBracketL -> 1
  TBracketR -> 1
  TAssign -> 1
  TPlus -> 1
  TMinus -> 1
  TMultiply -> 1
  TDivide -> 1
  TMod -> 1
  TLt -> 1
  TGt -> 1
  TEq -> 2
  TNotEqual -> 2
  TLe -> 2
  TGe -> 2
  TNot -> 1
  TKwLet -> 3
  TKwConst -> 5
  TKwIf -> 2
  TKwElse -> 4
  TKwElif -> 4
  TKwOn -> 2
  TKwKeyPress -> 9
  TKwCollision -> 9
  TKwUpdate -> 6
  TKwInteract -> 8
  TNewline -> 1
  TBlank -> 1
  TEOF -> 0
  TError s -> length s

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

-- | A specialized version of `createListPanel` for the Token Viewer.
-- It takes a list of `Token` objects and creates a `<span>` for each,
-- with data attributes for position and length.
createTokenPanel :: String -> String -> [Token] -> Element -> UI (Element, Element)
createTokenPanel title placeholderMsg initialTokens sourceArea = do
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
  updateTokenPanel content placeholderMsg initialTokens sourceArea
  pure (root, content)

-- | A specialized version of `updateListPanel` for the Token Viewer.
-- It takes a list of `Token` objects and creates a `<span>` for each,
-- with data attributes for position and length.
updateTokenPanel :: Element -> String -> [Token] -> Element -> UI ()
updateTokenPanel content placeholderMsg tokens sourceArea = do
  rows <- forM tokens $ \token -> do
    let pos = tokenPosition token
    let len = getTokenLength token
    let lineNum = line pos
    let colNum = column pos

    spanEl <- UI.span
    
    element spanEl # set UI.text (show token)
                   # set (UI.attr "data-line") (show lineNum)
                   # set (UI.attr "data-col") (show colNum)
                   # set (UI.attr "data-len") (show len)
                   # set UI.style [("cursor", "pointer")]
    on UI.hover spanEl $ \_ -> do
      source <- get value sourceArea
      let lines' = lines source
      let start = sum (map ((+1) . length) (take (lineNum - 1) lines')) + (colNum - 1)
      let end = start + len
      runFunction $ ffi "%1.focus(); %1.setSelectionRange(%2, %3)" sourceArea start end
    on UI.leave spanEl $ \_ ->
      void $ element sourceArea # set UI.selection Nothing

    UI.div #+ [element spanEl]

  finalRows <- if null tokens
    then do
      placeholder <- UI.div # set UI.text placeholderMsg
      return [placeholder]
    else
      return rows

  void $ element content # set children finalRows