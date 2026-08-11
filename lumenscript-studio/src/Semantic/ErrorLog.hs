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
-- TVoid covers event-handler bodies and function calls used as
-- statements (e.g. "player.jump()" has no value to consume).
-- TUnknown is a placeholder returned after an error has already been
-- logged, so the walk can keep going without triggering a cascade of
-- unrelated follow-on type errors from the same root cause.
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

-- | Reserved for future soft diagnostics (e.g. unused variables) that
-- shouldn't block bytecode generation. Every check currently in scope
-- for FR4 is Fatal.
data Severity = Warning | Fatal
  deriving (Eq, Show)

-- ---------------------------------------------------------------------
-- Diagnostics
-- ---------------------------------------------------------------------

-- | Every distinct semantic problem GameScript Studio can report,
-- covering both checking passes: ScopeResolution (Undefined*,
-- DuplicateDeclaration) and TypeCheck (the rest).
data SemanticError
  = UndefinedVariable     { errName :: String, errPos :: Position }
  | UndefinedFunction     { errName :: String, errPos :: Position }
  | UndefinedMember       { errBase :: String, errName :: String, errPos :: Position }
  | DuplicateDeclaration  { errName :: String, errPos :: Position, firstPos :: Position }
  | TypeMismatch          { expectedTy :: Type, actualTy :: Type, errContext :: String, errPos :: Position }
  | ArityMismatch         { errName :: String, expectedArgs :: Int, actualArgs :: Int, errPos :: Position }
  | InvalidOperandType    { operator :: String, operandTy :: Type, errPos :: Position }
  | InvalidCondition      { actualTy :: Type, errPos :: Position }
  | AssignmentTypeMismatch{ errName :: String, declaredTy :: Type, assignedTy :: Type, errPos :: Position }
  deriving (Eq, Show)

severityOf :: SemanticError -> Severity
severityOf _ = Fatal

-- | Human-readable form, styled to match the parser's
-- "<message> at <position>" error reporting so token, syntax, and
-- semantic errors all read consistently in the UI's error panel.
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
renderError (AssignmentTypeMismatch name declared assigned pos) =
  at pos ++ "cannot assign " ++ show assigned ++ " to '" ++ name ++ "' declared as " ++ show declared

at :: Position -> String
at pos = show pos ++ ": "

-- | All collected errors, one per line, in the order they were found --
-- what the Script Editor's error panel (FR4) actually displays.
renderErrors :: [SemanticError] -> String
renderErrors = intercalate "\n" . map renderError

-- | Whether bytecode generation should be blocked (FR4: semantic
-- errors must be reported "before proceeding to bytecode generation").
hasFatalErrors :: [SemanticError] -> Bool
hasFatalErrors = any ((== Fatal) . severityOf)

-- | No errors were logged at all.
semanticOk :: [SemanticError] -> Bool
semanticOk = null

-- ---------------------------------------------------------------------
-- SemanticM: an error-accumulating checker monad
--
-- A plain `Either SemanticError a`, like ParseResult, can't collect
-- more than one error -- `>>=` short-circuits on the first Left. Since
-- FR4 wants every semantic problem reported at once (not one typo per
-- recompile), SemanticM instead always keeps going, threading an
-- accumulated error list alongside the value being built. ScopeResolution
-- and TypeCheck each run one full AST walk in this monad; when a check
-- fails they log the error and return a placeholder (TUnknown, or the
-- expected type) so the walk continues cleanly instead of aborting.
-- ---------------------------------------------------------------------
newtype SemanticM a = SemanticM { runSM :: [SemanticError] -> (a, [SemanticError]) }

instance Functor SemanticM where
  fmap f (SemanticM g) = SemanticM $ \errs ->
    let (a, errs') = g errs in (f a, errs')

instance Applicative SemanticM where
  pure x = SemanticM $ \errs -> (x, errs)
  (SemanticM f) <*> (SemanticM g) = SemanticM $ \errs ->
    let (h, errs1) = f errs
        (a, errs2) = g errs1
    in (h a, errs2)

instance Monad SemanticM where
  return = pure
  (SemanticM g) >>= f = SemanticM $ \errs ->
    let (a, errs') = g errs
        SemanticM h = f a
    in h errs'

-- | Record a problem and keep going. The caller supplies whatever
-- placeholder value lets the walk continue, e.g.:
--
-- > checkVar name pos = case lookupVar name scope of
-- >   Just ty -> pure ty
-- >   Nothing -> do
-- >     logError (UndefinedVariable name pos)
-- >     pure TUnknown
logError :: SemanticError -> SemanticM ()
logError e = SemanticM $ \errs -> ((), errs ++ [e])

-- | Same mechanism as logError; kept as a distinct name so call sites
-- say what they mean, and so a future Warning severity can be filtered
-- out of hasFatalErrors without revisiting every call site.
logWarning :: SemanticError -> SemanticM ()
logWarning = logError

-- | Run a full semantic check pass, returning the final value (which
-- may be a best-effort result if errors were logged along the way)
-- plus every error collected, in the order they were found.
runSemanticM :: SemanticM a -> (a, [SemanticError])
runSemanticM m = runSM m []