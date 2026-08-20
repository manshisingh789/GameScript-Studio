module AST
  ( Ident
  , Expr(..)
  , UnaryOp(..)
  , BinOp(..)
  , EventSpec(..)
  , Stmt(..)
  , Program
  ) where

import Token (Position)

-- | An identifier, typically a variable or function name.
type Ident = String

-- | Represents an expression in the LumenScript language.
data Expr
  = LitInt Int                       -- ^ An integer literal, e.g., `42`.
  | LitStr String                    -- ^ A string literal, e.g., `"hello"`.
  | LitBool Bool                     -- ^ A boolean literal, e.g., `true`.
  | Var Ident Position               -- ^ A variable reference, e.g., `x`. Stores the identifier's position.
  | Member Expr Ident Position       -- ^ An object member access, e.g., `obj.field`. Stores the member's position.
  | Unary UnaryOp Expr Position      -- ^ A unary operation, e.g., `-x` or `!flag`. Stores the operator's position.
  | Binary BinOp Expr Expr Position  -- ^ A binary operation, e.g., `a + b`. Stores the operator's position.
  | Call Expr [Expr] Position        -- ^ A function call, e.g., `func(a, b)`. Stores the position of the opening parenthesis.
  deriving (Show, Eq)

-- | Unary operators.
data UnaryOp
  = Neg -- ^ Negation, e.g., `-x`.
  | Not -- ^ Logical not, e.g., `!flag`.
  deriving (Show, Eq)

-- | Binary operators.
data BinOp
  = Add | Sub | Mul | Div | Mod    -- ^ Arithmetic operators: +, -, *, /, %
  | Eq | NotEq | Lt | Gt | Le | Ge -- ^ Comparison operators: ==, !=, <, >, <=, >=
  | And | Or                       -- ^ Logical operators: &&, ||
  deriving (Show, Eq)

-- | Specifies the type of event for an `OnEvent` block.
-- Made a generic EventSpec if we decided to add more event types (future scope)
data EventSpec
  = KeyPress Ident -- ^ A key press event, e.g., `on key_press SPACE:`.
  | GenericEvent Ident [Ident] -- or EventName + parameters
  deriving (Show, Eq)

-- | Represents a statement in the LumenScript language.
data Stmt
  = Decl Ident Expr Position           -- ^ A variable declaration, e.g., `let x = 10;`. Stores the variable's position.
  | Assign Expr Expr Position         -- ^ An assignment to a variable, e.g., `x = 20;`. Stores the variable's position.
  | ExprStmt Expr                      -- ^ An expression used as a statement, typically a function call for its side effects.
  | If Expr [Stmt] (Maybe [Stmt]) Position -- ^ An if-else statement. Stores the position of the 'if' keyword.
  | OnEvent EventSpec [Stmt] Position  -- ^ An event handler block, e.g., `on key_press SPACE: { ... }`. Stores the position of the 'on' keyword.
  deriving (Show, Eq)

-- | A program is a list of statements.
type Program = [Stmt]