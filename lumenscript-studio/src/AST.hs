module AST where

import Token (Position)

type Ident = String

-- ===== Expressions (Person A) =====

data Expr
  = LitInt Int
  | LitStr String
  | LitBool Bool
  | Var Ident Position               -- x                 (position of the identifier)
  | Member Expr Ident Position       -- player.distance  ->  Member (Var "player" p1) "distance" p2
  | Unary UnaryOp Expr Position      -- -x, !flag         (position of the operator)
  | Binary BinOp Expr Expr Position  -- a + b, x <= y     (position of the operator)
  | Call Expr [Expr] Position        -- player.jump(1, 2) -> Call (Member (Var "player" p1) "jump" p2) [..] p3
  deriving (Show, Eq)                -- (position of the opening paren -- the call site itself)

data UnaryOp = Neg | Not
  deriving (Show, Eq)

data BinOp
  = Add | Sub | Mul | Div | Mod
  | Eq | NotEq | Lt | Gt | Le | Ge
  deriving (Show, Eq)

-- ===== Statements (Person B) =====

data EventSpec = KeyPress Ident      -- key_press SPACE
  deriving (Show, Eq)

data Stmt
  = Decl Ident Expr Position           -- let x = expr        (position of x)
  | Assign Ident Expr Position         -- x = expr            (position of x)
  | ExprStmt Expr                      -- player.jump()  (bare call as a statement -- position lives on the inner Call)
  | If Expr [Stmt] (Maybe [Stmt]) Position -- if cond: { ... } [else: { ... }] (position of 'if' -- a fallback for
                                            -- when cond is a bare literal, which carries no position of its own)
  | OnEvent EventSpec [Stmt] Position  -- on key_press SPACE: { ... } (position of the 'on' keyword -- the binding site)
  deriving (Show, Eq)

type Program = [Stmt]