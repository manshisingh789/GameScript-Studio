module GUI.ASTViewer
  ( ASTViewer(..)
  , createASTViewer
  , updateASTViewer
  , dummyAstLines
  ) where

import Graphics.UI.Threepenny.Core (Element, UI)
import GUI.Panel (createListPanel, updateListPanel)

-- | The AST viewer panel. Shows 'dummyAstLines' only until the first
-- compile -- GUI.App wires the "Compile" button to call
-- `updateASTViewer viewer (map show realProgram)` using the real
-- output of Parser.parseProgram, so after that this panel reflects
-- the editor's actual parsed AST (or a real parse error, if parsing
-- fails).
data ASTViewer = ASTViewer
  { avRoot    :: Element
  , avContent :: Element
  }

createASTViewer :: UI ASTViewer
createASTViewer = do
  (root, content) <- createListPanel "AST" placeholderMsg dummyAstLines
  pure (ASTViewer root content)

updateASTViewer :: ASTViewer -> [String] -> UI ()
updateASTViewer av = updateListPanel (avContent av) placeholderMsg

placeholderMsg :: String
placeholderMsg = "(no AST yet -- compile a script to see it here)"

-- | Static placeholder rows shown before real AST data is wired up.
dummyAstLines :: [String]
dummyAstLines =
  [ "Decl \"x\" (LitInt 5) <pos>"
  , "ExprStmt (Call (Member (Var \"player\") \"jump\") [] <pos>)"
  , "OnEvent (KeyPress \"Space\") [ ... ] <pos>"
  ]