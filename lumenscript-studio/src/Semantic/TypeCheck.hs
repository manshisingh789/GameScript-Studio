module Semantic.TypeCheck
  ( typeCheckProgram
  , checkStmts
  , checkStmt
  , inferExpr
  , analyzeProgram
  ) where

import Control.Monad (foldM)

import AST
import Token (Position)
import Semantic.ErrorLog
import Semantic.SymbolTable
import Semantic.ScopeResolution (resolveStmts)

-- | Entry point: type-check a whole program and return every type
-- error found, in source order.
--
-- Assumes ScopeResolution has already run (or will be run alongside
-- this in the pipeline) -- this pass does not re-report undefined
-- names, only type problems, to avoid the same root cause producing
-- two different error messages. It builds its own fresh, type-aware
-- symbol table rather than reusing ScopeResolution's, since that one
-- only knows *that* each name exists, not what type it holds -- see
-- the note at the top of ScopeResolution.hs.
typeCheckProgram :: Program -> [SemanticError]
typeCheckProgram stmts = snd (runSemanticM (checkStmts emptyTable stmts))

-- | Runs scope resolution and type checking, returning errors.
analyzeProgram :: Program -> SemanticM ()
analyzeProgram prog = do
  -- 1. Run scope resolution
  resolvedTable <- resolveStmts emptyTable prog
  -- 2. Run type checking
  _ <- checkStmts emptyTable prog
  pure ()

-- | Type-check a sequence of statements left-to-right, threading the
-- (now type-aware) symbol table forward.
checkStmts :: SymbolTable -> [Stmt] -> SemanticM SymbolTable
checkStmts = foldM checkStmt

checkStmt :: SymbolTable -> Stmt -> SemanticM SymbolTable
checkStmt st (Decl name rhs pos) = do
  ty <- inferExpr st rhs
  case declare name ty pos st of
    Left _firstPos -> pure st   -- duplicate declaration already reported by ScopeResolution
    Right st'       -> pure st'
checkStmt st (Assign name rhs pos) = do
  actualTy <- inferExpr st rhs
  case lookupSymbol name st of
    Nothing   -> pure st        -- undefined variable already reported by ScopeResolution
    Just info
      | symbolType info == actualTy
        -> pure st
      | symbolType info == TUnknown
      || actualTy == TUnknown
        -> pure st
      | otherwise ->
          logError (AssignmentTypeMismatch name (symbolType info) actualTy pos) >> pure st
checkStmt st (ExprStmt e) = do
  _ <- inferExpr st e
  pure st
checkStmt st (If cond thenBlock elseBlock ifPos) = do
  condTy <- inferExpr st cond
  case condTy of
    TBool    -> pure ()
    TUnknown -> pure ()          -- already reported by whatever made cond unresolved
    other    -> logError (InvalidCondition other (exprPosOr ifPos cond))
  -- Each branch gets its own scope; declarations inside it don't leak
  -- back out, matching ScopeResolution's handling of If.
  _ <- checkStmts (enterScope st) thenBlock
  case elseBlock of
    Nothing -> pure ()
    Just eb -> () <$ checkStmts (enterScope st) eb
  pure st
checkStmt st (OnEvent _event body _pos) = do
  _ <- checkStmts (enterScope st) body
  pure st

-- | Infer the Type of an expression, logging any type errors found
-- along the way. Returns TUnknown wherever the "real" type can't be
-- determined -- either because this expression is itself broken, or
-- because it depends on something ScopeResolution already flagged --
-- so that one root cause doesn't cascade into a wall of follow-on
-- errors about types that were never resolvable in the first place.
inferExpr :: SymbolTable -> Expr -> SemanticM Type
inferExpr _ (LitInt _)  = pure TInt
inferExpr _ (LitStr _)  = pure TString
inferExpr _ (LitBool _) = pure TBool
inferExpr st (Var name _pos) =
  case lookupSymbol name st of
    Just info -> pure (symbolType info)
    Nothing   -> pure TUnknown  -- undefined (or a bare built-in object name); ScopeResolution's problem, not ours
inferExpr st (Member baseExpr field _pos) = do
  _ <- inferExpr st baseExpr   -- surface any error nested inside the base, even though we don't need its type here
  case baseName baseExpr of
    Just base | isBuiltinObject base ->
      case lookupMember base field of
        Just ty -> pure ty
        Nothing -> pure TUnknown  -- unknown member; ScopeResolution's problem
    _ -> pure TUnknown            -- base isn't a recognized object; nothing to infer
inferExpr st (Call callee args pos) = do
  argTys   <- mapM (inferExpr st) args
  calleeTy <- inferExpr st callee
  case calleeTy of
    TFunction paramTys retTy -> do
      checkArgTypes paramTys argTys args pos (calleeLabel callee)
      pure retTy
    TUnknown -> pure TUnknown      -- unresolved callee; ScopeResolution's problem
    other    -> do
      logError (TypeMismatch (TFunction [] TUnknown) other ("calling " ++ calleeLabel callee) pos)
      pure TUnknown
inferExpr st (Unary op operand pos) = do
  ty <- inferExpr st operand
  case (op, ty) of
    (Neg, TInt)     -> pure TInt
    (Neg, TUnknown) -> pure TUnknown
    (Neg, other)    -> logError (InvalidOperandType "-" other pos) >> pure TUnknown
    (Not, TBool)    -> pure TBool
    (Not, TUnknown) -> pure TUnknown
    (Not, other)    -> logError (InvalidOperandType "!" other pos) >> pure TUnknown
inferExpr st (Binary op lhs rhs pos) = do
  lty <- inferExpr st lhs
  rty <- inferExpr st rhs
  inferBinary op lty rty pos

-- | Type rules per operator family:
--
--   * arithmetic (+ - * / %)     : int, int -> int
--   * equality   (== !=)         : any matching pair of types -> bool
--                                   (lets scripts compare strings/bools too,
--                                   e.g. checking a dialogue flag)
--   * relational (< > <= >=)     : int, int -> bool  (the only ordered type)
--
-- TUnknown on either side always short-circuits to TUnknown without
-- logging -- that operand's problem was already reported elsewhere.
inferBinary :: BinOp -> Type -> Type -> Position -> SemanticM Type
inferBinary _ TUnknown _ _ = pure TUnknown
inferBinary _ _ TUnknown _ = pure TUnknown
inferBinary op lty rty pos
  | op `elem` [Add, Sub, Mul, Div, Mod] =
      if lty == TInt && rty == TInt
        then pure TInt
        else logError (InvalidOperandType (opSymbol op) (worseOperand lty rty) pos) >> pure TUnknown
  | op `elem` [Eq, NotEq] =
      if lty == rty
        then pure TBool
        else logError (TypeMismatch lty rty "comparison" pos) >> pure TBool
        -- still yields TBool even though the comparison is nonsensical,
        -- so an "if a == b:" using this doesn't also trip InvalidCondition
  | op `elem` [Lt, Gt, Le, Ge] =
      if lty == TInt && rty == TInt
        then pure TBool
        else logError (InvalidOperandType (opSymbol op) (worseOperand lty rty) pos) >> pure TBool
  | otherwise = pure TUnknown  -- exhaustive over BinOp in practice; kept for totality

-- | Which side to blame when a binary operand type check fails: the
-- one that isn't the expected TInt (arbitrarily the left, if somehow
-- both are wrong -- one message is enough).
worseOperand :: Type -> Type -> Type
worseOperand lty rty
  | lty /= TInt = lty
  | otherwise   = rty

opSymbol :: BinOp -> String
opSymbol Add    = "+"
opSymbol Sub    = "-"
opSymbol Mul    = "*"
opSymbol Div    = "/"
opSymbol Mod    = "%"
opSymbol Eq     = "=="
opSymbol NotEq  = "!="
opSymbol Lt     = "<"
opSymbol Gt     = ">"
opSymbol Le     = "<="
opSymbol Ge     = ">="

-- | Check each call argument's inferred type against the callee's
-- declared parameter types. Silently skipped on an arity mismatch --
-- ScopeResolution already reported that, and zipping mismatched-length
-- lists would either crash or produce misleading partial checks.
checkArgTypes :: [Type] -> [Type] -> [Expr] -> Position -> String -> SemanticM ()
checkArgTypes paramTys argTys argExprs pos label
  | length paramTys /= length argTys = pure ()
  | otherwise = mapM_ checkOne (zip3 paramTys argTys argExprs)
  where
    checkOne (expected, actual, argExpr)
      | expected == actual || actual == TUnknown = pure ()
      | otherwise =
          logError (TypeMismatch expected actual ("argument to " ++ label) (exprPosOr pos argExpr))

-- | A human-readable name for a callee, for TypeMismatch/error text --
-- "player.jump" for a method, the bare name for a plain variable call,
-- and a generic fallback for anything else the grammar doesn't
-- currently produce.
calleeLabel :: Expr -> String
calleeLabel (Member baseExpr field _) = case baseName baseExpr of
  Just base -> base ++ "." ++ field
  Nothing   -> field
calleeLabel (Var name _) = name
calleeLabel _             = "<expression>"

-- | The immediate identifier a Member/Call chain is rooted on, if it's
-- a simple one-level access like "player.jump". Deliberately
-- duplicated from ScopeResolution.hs rather than imported -- these two
-- modules are meant to be independently readable/testable passes over
-- the same AST (see the note at the top of this file), so each stays
-- self-contained rather than reaching into the other's internals.
baseName :: Expr -> Maybe String
baseName (Var name _) = Just name
baseName _             = Nothing

-- | The position an expression itself carries, if any -- literals
-- carry none. Used to point at the specific sub-expression that's
-- wrong (an argument, a condition) when one is available.
exprPos :: Expr -> Maybe Position
exprPos (LitInt _)      = Nothing
exprPos (LitStr _)      = Nothing
exprPos (LitBool _)     = Nothing
exprPos (Var _ p)       = Just p
exprPos (Member _ _ p)  = Just p
exprPos (Call _ _ p)    = Just p
exprPos (Binary _ _ _ p) = Just p
exprPos (Unary _ _ p)   = Just p

-- | exprPos with a fallback position (e.g. the call site, or the
-- enclosing 'if') for the literal case, which has none of its own.
exprPosOr :: Position -> Expr -> Position
exprPosOr fallback e = maybe fallback id (exprPos e)