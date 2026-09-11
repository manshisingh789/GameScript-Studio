module GUI.TokenViewer
  ( TokenViewer(..)
  , createTokenViewer
  , updateTokenViewer
  , dummyTokens
  , dummyTokenLines
  ) where

import Graphics.UI.Threepenny.Core (Element, UI)
import GUI.Panel (createTokenPanel, updateTokenPanel)
import Token (Token(..), TokenPayload(..), Position(..))

-- | The token list panel. Shows 'dummyTokens' only until the
-- first compile -- GUI.App wires the "Compile" button to call
-- `updateTokenViewer viewer realTokens` using the real
-- output of Lexer.lexer, so after that this panel reflects the
-- editor's actual token stream.
data TokenViewer = TokenViewer
  { tvRoot    :: Element
  , tvContent :: Element
  }

createTokenViewer :: Element -> UI TokenViewer
createTokenViewer sourceArea = do
  (root, content) <- createTokenPanel "Tokens" placeholderMsg dummyTokens sourceArea
  pure (TokenViewer root content)

-- | Replace the panel's rows with one line per entry in `tokens`.
updateTokenViewer :: TokenViewer -> [Token] -> Element -> UI ()
updateTokenViewer tv = updateTokenPanel (tvContent tv) placeholderMsg

placeholderMsg :: String
placeholderMsg = "(no tokens yet -- compile a script to see them here)"

-- | Static placeholder rows shown before real token data is wired up.
dummyTokens :: [Token]
dummyTokens =
  [ Token (TKwLet) (Position 1 1)
  , Token (TIdent "x") (Position 1 5)
  , Token (TAssign) (Position 1 7)
  , Token (TInt 5) (Position 1 9)
  ]

dummyTokenLines :: [String]
dummyTokenLines = map show dummyTokens