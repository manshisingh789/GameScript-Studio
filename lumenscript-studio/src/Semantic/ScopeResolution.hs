module Semantic.ScopeResolution
  ( resolveProgram
  , resolveStmts
  , resolveStmt
  , resolveExpr
  ) where

import Control.Monad (foldM)

import AST
import Token (Position)
import Semantic.ErrorLog
import Semantic.SymbolTable

-- | Entry point: walk a whole program and return every scope-related
-- error found (UndefinedVariable, UndefinedFunction, UndefinedMember,
-- DuplicateDeclaration, ArityMismatch), in source order.
--
-- This pass only cares whether names resolve, not what type they carry
-- -- declarations here go into the table as TUnknown. TypeCheck.hs
-- performs its own walk afterwards with a fresh table, declaring each
-- name with its real inferred type; that keeps scope-checking and
-- type-checking as two independently testable passes instead of one
-- tangled one.
resolveProgram :: Program -> [SemanticError]
resolveProgram stmts = snd (runSemanticM (resolveStmts emptyTable stmts))

-- | Resolve a sequence of statements left-to-right, threading the
-- symbol table forward so later statements see earlier "let"s.
resolveStmts :: SymbolTable -> [Stmt] -> SemanticM SymbolTable
resolveStmts = foldM resolveStmt

resolveStmt :: SymbolTable -> Stmt -> SemanticM SymbolTable
resolveStmt st (Decl name rhs pos) = do
  resolveExpr st rhs
  case declare name TUnknown pos st of
    Left firstPos -> do
      logError (DuplicateDeclaration name pos firstPos)
      pure st                 -- keep the original binding; don't let a
                               -- bad redeclaration corrupt later lookups
    Right st' -> pure st'
resolveStmt st (Assign name rhs pos) = do
  resolveExpr st rhs
  case lookupSymbol name st of
    Just _  -> pure ()
    Nothing -> logError (UndefinedVariable name pos)
    -- Note: assigning to a built-in object name (e.g. "player = 5") is
    -- also caught here, since built-ins are never in the variable
    -- table -- it just currently reads as "undefined variable" rather
    -- than a more specific "player is not assignable" message.
  pure st
resolveStmt st (ExprStmt e) = do
  resolveExpr st e
  pure st
resolveStmt st (If cond thenBlock elseBlock _ifPos) = do
  resolveExpr st cond
  -- Each branch gets its own scope; declarations inside an if/else
  -- block don't leak back out, so the table returned to the caller is
  -- the original st, unchanged.
  _ <- resolveStmts (enterScope st) thenBlock
  case elseBlock of
    Nothing -> pure ()
    Just eb -> () <$ resolveStmts (enterScope st) eb
  pure st
resolveStmt st (OnEvent _event body _pos) = do
  -- Event bodies are their own scope, same reasoning as If above.
  _ <- resolveStmts (enterScope st) body
  pure st

-- | Resolve every name reference inside an expression. Doesn't return
-- a Type -- that's TypeCheck's job -- just logs anything unresolved.
resolveExpr :: SymbolTable -> Expr -> SemanticM ()
resolveExpr _  (LitInt _)  = pure ()
resolveExpr _  (LitStr _)  = pure ()
resolveExpr _  (LitBool _) = pure ()
resolveExpr st (Var name pos) =
  case lookupSymbol name st of
    Just _ -> pure ()
    Nothing
      | isBuiltinObject name -> pure ()  -- "player" etc. used bare; not
                                          -- this pass's job to say
                                          -- whether that's meaningful
      | otherwise -> logError (UndefinedVariable name pos)
resolveExpr st (Member baseExpr field pos) = do
  resolveExpr st baseExpr
  case baseName baseExpr of
    Just base | isBuiltinObject base ->
      case lookupMember base field of
        Just _  -> pure ()
        Nothing -> logError (UndefinedMember base field pos)
    -- Base isn't a recognized built-in object (either it's a declared
    -- variable, in which case member access on a scalar is a TypeCheck
    -- concern, or it was already flagged as undefined above).
    _ -> pure ()
resolveExpr st (Call callee args pos) = do
  mapM_ (resolveExpr st) args
  resolveExpr st callee
  checkArity st callee args pos
resolveExpr st (Unary _ operand _) =
  resolveExpr st operand
resolveExpr st (Binary _ lhs rhs _) = do
  resolveExpr st lhs
  resolveExpr st rhs

-- | If the callee is base.field on a known built-in method, verify the
-- argument count matches. Left silent (not this pass's problem) when
-- the callee shape isn't base.field, or the base/field itself is
-- already unresolved -- resolveExpr on the callee already reported
-- that, and reporting an arity mismatch on top would be noise about
-- the same root cause.
checkArity :: SymbolTable -> Expr -> [Expr] -> Position -> SemanticM ()
checkArity _ (Member baseExpr field _) args pos =
  case baseName baseExpr of
    Just base | isBuiltinObject base ->
      case lookupMember base field of
        Just (TFunction params _)
          | length params /= length args ->
              logError (ArityMismatch (base ++ "." ++ field) (length params) (length args) pos)
        Just _  -> pure ()  -- property, not callable -- TypeCheck reports that
        Nothing -> pure ()  -- unknown member -- already reported by resolveExpr
    _ -> pure ()
checkArity _ _ _ _ = pure ()  -- calling something that isn't base.field
                               -- isn't representable by the current grammar

-- | The immediate identifier a Member/Call chain is rooted on, if it's
-- a simple one-level access like "player.jump" -- current built-ins
-- are all single-level, so deeper chains (e.g. "a.b.c") intentionally
-- fall through unresolved here rather than guessing.
baseName :: Expr -> Maybe String
baseName (Var name _) = Just name
baseName _             = Nothing