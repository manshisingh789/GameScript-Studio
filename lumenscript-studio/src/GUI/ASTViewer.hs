module GUI.ASTViewer
  ( ASTViewer(..)
  , createASTViewer
  , updateASTViewer
    , dummyAstLines
  ) where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Control.Monad (void)

import AST ( Program, Stmt(..), Expr(..) )
import GUI.Panel (createListPanel)

-- | The AST viewer panel.
data ASTViewer = ASTViewer
  { avRoot    :: Element
  , avContent :: Element
  }

createASTViewer :: UI ASTViewer
createASTViewer = do
  (root, content) <- createListPanel "AST" placeholderMsg []
  pure (ASTViewer root content)

updateASTViewer :: ASTViewer -> Either String Program -> UI ()
updateASTViewer av (Left err) = do
    let content = avContent av
    void $ element content # set children []
    void $ element content #+ [UI.div #. "error" #+ [string err]]
updateASTViewer av (Right program) = do
  treeHtml <- programToTreeUI program
  let content = avContent av
  void $ element content # set children []
  void $ element content #+ [return treeHtml]

placeholderMsg :: String
placeholderMsg = "(no AST yet -- compile a script to see it here)"

dummyAstLines :: [String]
dummyAstLines = ["Program"]

-- Main rendering functions
programToTreeUI :: Program -> UI Element
programToTreeUI stmts = renderNode "Program" (map stmtToTreeUI stmts)

stmtToTreeUI :: Stmt -> UI Element
stmtToTreeUI stmt = case stmt of
    Decl ident mutability expr _ -> renderNode "Declaration"
        [ renderLeaf (show ident)
        , renderLeaf (show mutability)
        , exprToTreeUI expr
        ]
    Assign lhs rhs _ -> renderNode "Assignment" [exprToTreeUI lhs, exprToTreeUI rhs]
    ExprStmt expr -> renderNode "ExprStmt" [exprToTreeUI expr]
    If cond thenB (Just elseB) _ -> renderNode "If"
        [ renderNode "Condition" [exprToTreeUI cond]
        , renderNode "Then" (map stmtToTreeUI thenB)
        , renderNode "Else" (map stmtToTreeUI elseB)
        ]
    If cond thenB Nothing _ -> renderNode "If"
        [ renderNode "Condition" [exprToTreeUI cond]
        , renderNode "Then" (map stmtToTreeUI thenB)
        ]
    OnEvent spec body _ -> renderNode "Event"
        [ renderLeaf (show spec)
        , renderNode "Body" (map stmtToTreeUI body)
        ]

exprToTreeUI :: Expr -> UI Element
exprToTreeUI expr = case expr of
    LitInt i -> renderLeaf $ "IntLit " ++ show i
    LitStr s -> renderLeaf $ "StrLit " ++ show s
    LitBool b -> renderLeaf $ "BoolLit " ++ show b
    Var ident _ -> renderLeaf $ "Var " ++ ident
    Member e ident _ -> renderNode "MemberAccess" [exprToTreeUI e, renderLeaf ident]
    Unary op e _ -> renderNode (show op) [exprToTreeUI e]
    Binary op e1 e2 _ -> renderNode (show op) [exprToTreeUI e1, exprToTreeUI e2]
    Call e args _ -> renderNode "FunctionCall" (exprToTreeUI e : map exprToTreeUI args)

-- Helper for rendering a node with children
renderNode :: String -> [UI Element] -> UI Element
renderNode label children = do
  toggleButton <- UI.button
    # set UI.text "[-]"
    # set UI.style
        [ ("width", "32px")
        , ("margin-right", "4px")
        , ("padding", "0px")
        ]
  labelSpan <- UI.span #. "tree-label"
    # set UI.style
      [ ("cursor", "pointer")
      , ("display", "block")
      , ("padding", "2px 5px")
      ]
    #+ [string label]
  header <- UI.div #. "tree-header"
    # set UI.style [("display", "flex"), ("align-items", "center")]
    #+ [element toggleButton, element labelSpan]
  childrenDiv <- UI.div #. "tree-children"
    # set UI.style
      [ ("display", "block")
      , ("border-left", "1px solid #ccc")
      , ("margin-left", "10px")
      , ("padding-left", "10px")
      ]
    #+ children
    
  on UI.click toggleButton . const $
      void $ toggleClass "collapsed" childrenDiv toggleButton

  UI.div #. "tree-node"
    # set UI.style [("margin-left", "20px")]
    #+ [element header, element childrenDiv]

-- Helper for rendering a leaf node (no children)
renderLeaf :: String -> UI Element
renderLeaf label = do
  labelSpan <- UI.span #. "tree-label"
    # set UI.style
      [ ("cursor", "pointer")
      , ("display", "block")
      , ("padding", "2px 5px")
      ]
    #+ [string label]
  UI.div #. "tree-node tree-leaf"
    # set UI.style [("margin-left", "20px")]
    #+ [element labelSpan]

-- Helper to toggle a class on an element
toggleClass :: String -> Element -> Element -> UI Element
toggleClass className el toggleButton = do
  runFunction $ ffi
    "if (%1.classList.toggle(%2)) { %1.style.display = 'none'; %3.textContent = '[+]'; } else { %1.style.display = 'block'; %3.textContent = '[-]'; }"
    el className toggleButton
  pure el