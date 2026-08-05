module AST where

type Ident = String

-- ===== Expressions (Person A) =====

data Expr
  = LitInt Int
  | LitStr String
  | LitBool Bool
  | Var Ident                    -- x
  | Member Expr Ident            -- player.distance  ->  Member (Var "player") "distance"
  | Unary UnaryOp Expr           -- -x, !flag
  | Binary BinOp Expr Expr       -- a + b, x <= y
  | Call Expr [Expr]             -- player.jump(1, 2) -> Call (Member (Var "player") "jump") [..]
  deriving (Show, Eq)

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
  = Decl Ident Expr                  -- let x = expr
  | Assign Ident Expr                -- x = expr
  | ExprStmt Expr                    -- player.jump()  (bare call as a statement)
  | If Expr [Stmt] (Maybe [Stmt])    -- if cond: { ... } [else: { ... }]
  | OnEvent EventSpec [Stmt]         -- on key_press SPACE: { ... }
  deriving (Show, Eq)

type Program = [Stmt]