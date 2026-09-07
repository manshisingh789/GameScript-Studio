module GUI.TokenViewer
  ( TokenViewer(..)
  , createTokenViewer
  , updateTokenViewer
  , dummyTokenLines
  ) where

import Graphics.UI.Threepenny.Core (Element, UI)
import GUI.Panel (createListPanel, updateListPanel)

-- | The token list panel. Shows 'dummyTokenLines' only until the
-- first compile -- GUI.App wires the "Compile" button to call
-- `updateTokenViewer viewer (map show realTokens)` using the real
-- output of Lexer.lexer, so after that this panel reflects the
-- editor's actual token stream.
data TokenViewer = TokenViewer
  { tvRoot    :: Element
  , tvContent :: Element
  }

createTokenViewer :: UI TokenViewer
createTokenViewer = do
  (root, content) <- createListPanel "Tokens" placeholderMsg dummyTokenLines
  pure (TokenViewer root content)

-- | Replace the panel's rows with one line per entry in `tokenLines`.
updateTokenViewer :: TokenViewer -> [String] -> UI ()
updateTokenViewer tv = updateListPanel (tvContent tv) placeholderMsg

placeholderMsg :: String
placeholderMsg = "(no tokens yet -- compile a script to see them here)"

-- | Static placeholder rows shown before real token data is wired up.
dummyTokenLines :: [String]
dummyTokenLines =
  [ "KW_LET      \"let\""
  , "IDENT       \"x\""
  , "OP_ASSIGN   \"=\""
  , "INT_LIT     \"5\""
  , "SEPARATOR"
  ]