module Semantic.ErrorLog
  ( -- * Types the checker reasons about
    Type(..)
  , Severity(..)
    -- * Diagnostics
  , SemanticError(..)
  , renderError
  , renderErrors
  , hasFatalErrors
  , semanticOk
    -- * The checker monad
  , SemanticM
  , runSemanticM
  , logError
  , logWarning
  ) where

import Token (Position)
import Data.List (intercalate)

-- ---------------------------------------------------------------------
-- Types
-- ---------------------------------------------------------------------

-- | The value types GameScript's semantic analyzer reasons about.
data Type
  = TInt
  | TString
  | TBool
  | TVoid
  | TUnknown
  | TFunction [Type] Type
  deriving (Eq)

instance Show Type where
  show TInt            = "int"
  show TString         = "string"
  show TBool           = "bool"
  show TVoid           = "void"
  show TUnknown        = "<unknown>"
  show (TFunction as r) = "(" ++ intercalate ", " (map show as) ++ ") -> " ++ show r

-- | All semantic checks produce a diagnostic with a severity.
-- Fatal errors will block bytecode generation; warnings will not.
data Severity = Warning | Fatal
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------

-- | Every distinct semantic problem the compiler can report.
data SemanticError
  = UndefinedVariable     { errName :: String, errPos :: Position }
  | UndefinedFunction     { errName :: String, errPos :: Position }
  | UndefinedMember       { errBase :: String, errName :: String, errPos :: Position }
  | DuplicateDeclaration  { errName :: String, errPos :: Position, firstPos :: Position }
  | TypeMismatch          { expectedTy :: Type, actualTy :: Type, errContext :: String, errPos :: Position }
  | ArityMismatch         { errName :: String, expectedArgs :: Int, actualArgs :: Int, errPos :: Position }
  | InvalidOperandType    { operator :: String, operandTy :: Type, errPos :: Position }
  | InvalidCondition      { actualTy :: Type, errPos :: Position }
  | InvalidAssignmentTarget { errPos :: Position }
  | AssignmentTypeMismatch{ errName :: String, declaredTy :: Type, assignedTy :: Type, errPos :: Position }
  -- A generic warning for non-fatal issues, like unused variables.
  | GenericWarning        { message :: String, errPos :: Position }
  deriving (Eq, Show)

-- | Determines if an error should block compilation.
severityOf :: SemanticError -> Severity
severityOf (GenericWarning {}) = Warning
severityOf _                   = Fatal

-- | Human-readable form, styled to match the parser's error reporting.
renderError :: SemanticError -> String
renderError (UndefinedVariable name pos) =
  at pos ++ "undefined variable '" ++ name ++ "'"
renderError (UndefinedFunction name pos) =
  at pos ++ "undefined function '" ++ name ++ "'"
renderError (UndefinedMember base name pos) =
  at pos ++ "'" ++ base ++ "' has no member '" ++ name ++ "'"
renderError (DuplicateDeclaration name pos firstPos') =
  at pos ++ "'" ++ name ++ "' is already declared (first declared " ++ show firstPos' ++ ")"
renderError (TypeMismatch expected actual ctx pos) =
  at pos ++ ctx ++ ": expected " ++ show expected ++ ", got " ++ show actual
renderError (ArityMismatch name expected actual pos) =
  at pos ++ "'" ++ name ++ "' expects " ++ show expected ++ " argument(s), got " ++ show actual
renderError (InvalidOperandType op ty pos) =
  at pos ++ "operator '" ++ op ++ "' cannot be applied to " ++ show ty
renderError (InvalidCondition ty pos) =
  at pos ++ "condition must be bool, got " ++ show ty
renderError (InvalidAssignmentTarget pos) =
  at pos ++ "invalid assignment target"
renderError (AssignmentTypeMismatch name declared assigned pos) =
  at pos ++ "cannot assign " ++ show assigned ++ " to '" ++ name ++ "' declared as " ++ show declared
renderError (GenericWarning msg pos) =
  at pos ++ "warning: " ++ msg

at :: Position -> String
at pos = show pos ++ ": "

-- | Renders all collected errors, one per line.
renderErrors :: [SemanticError] -> String
renderErrors = intercalate "\n" . map renderError

-- | Checks if any logged errors are fatal.
hasFatalErrors :: [SemanticError] -> Bool
hasFatalErrors = any ((== Fatal) . severityOf)

-- | Checks if no errors were logged.
semanticOk :: [SemanticError] -> Bool
semanticOk = null

-- ---------------------------------------------------------------------
-- SemanticM: an error-accumulating checker monad
--
-- This is a custom implementation of a Writer monad, specialized for
-- accumulating a list of `SemanticError`s.
--
-- A standard `Either SemanticError a` monad would short-circuit on the
-- first error. This monad allows the semantic analysis passes to log
-- multiple errors in a single traversal of the AST.
-- ---------------------------------------------------------------------
newtype SemanticM a = SemanticM { runSM :: [SemanticError] -> (a, [SemanticError]) }

instance Functor SemanticM where
  -- fmap :: (a -> b) -> SemanticM a -> SemanticM b
  -- Applies a pure function to the value inside the monad, leaving the error log untouched.
  fmap f (SemanticM g) = SemanticM $ \errs ->
    let (a, errs') = g errs in (f a, errs')

instance Applicative SemanticM where
  -- pure :: a -> SemanticM a
  -- Lifts a pure value into the monad without adding any errors.
  pure x = SemanticM $ \errs -> (x, errs)

  -- (<*>) :: SemanticM (a -> b) -> SemanticM a -> SemanticM b
  -- Applies a function from within the monad to a value from within the monad,
  -- sequencing the errors from both.
  (SemanticM f) <*> (SemanticM g) = SemanticM $ \errs ->
    let (h, errs1) = f errs
        (a, errs2) = g errs1
    in (h a, errs2)

instance Monad SemanticM where
  return = pure

  -- (>>=) :: SemanticM a -> (a -> SemanticM b) -> SemanticM b
  -- Chains two monadic computations together, passing the result of the first
  -- to the second, and accumulating errors from both.
  (SemanticM g) >>= f = SemanticM $ \errs ->
    let (a, errs') = g errs
        SemanticM h = f a
    in h errs'

-- | Records a fatal error and allows the AST walk to continue.
logError :: SemanticError -> SemanticM ()
logError e = SemanticM $ \errs -> ((), errs ++ [e])

-- | Records a non-fatal warning.
logWarning :: String -> Position -> SemanticM ()
logWarning msg pos = SemanticM $ \errs -> ((), errs ++ [GenericWarning msg pos])

-- | Runs a semantic analysis pass, returning the final value and all collected errors.
runSemanticM :: SemanticM a -> (a, [SemanticError])
runSemanticM m = runSM m []