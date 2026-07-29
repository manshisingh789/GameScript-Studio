module AST where

type Ident = String

data Expr
  = LitInt Int
  | LitStr String
  | Member Ident Ident          -- player.distance
  deriving (Show, Eq)

data CompOp = Lt | Gt | Eq | Le | Ge
  deriving (Show, Eq)

data Cond = Cond Expr CompOp Expr    -- player.distance < 10
  deriving (Show, Eq)

data EventSpec = KeyPress Ident      -- key_press SPACE
  deriving (Show, Eq)

data Stmt
  = Call Ident Ident [Expr]          -- player.jump() / npc.say("...")
  | If Cond Stmt (Maybe Stmt)
  | OnEvent EventSpec Stmt
  deriving (Show, Eq)

type Program = [Stmt]