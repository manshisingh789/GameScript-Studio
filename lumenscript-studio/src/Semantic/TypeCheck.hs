module Semantic.TypeCheck
  ( analyzeProgram
  , typeCheckStmts
  , typeCheckStmt
  , inferExpr
  ) where

import Control.Monad (foldM, when)

import AST
import Token (Position)
import Semantic.ErrorLog
import Semantic.SymbolTable
import Semantic.ScopeResolution (resolveStmts)

-- | Runs scope resolution and then type checking, returning all errors found.
-- This is the main entry point for semantic analysis.
analyzeProgram :: Program -> SemanticM ()
analyzeProgram prog = do
  -- Pass 1: Scope Resolution. This builds a symbol table with information
  -- about which names are declared in which scope.
  scopedTable <- resolveStmts emptyTable prog

  -- Pass 2: Type Checking. This uses the scope information to verify types,
  -- building its own, more detailed, type-aware symbol table.
  _ <- typeCheckStmts scopedTable prog
  pure ()

-- | Type-check a sequence of statements left-to-right, threading the
-- (now type-aware) symbol table forward.
typeCheckStmts :: SymbolTable -> [Stmt] -> SemanticM SymbolTable
typeCheckStmts = foldM typeCheckStmt

-- | Type-check a single statement, updating the symbol table and logging errors.
typeCheckStmt :: SymbolTable -> Stmt -> SemanticM SymbolTable
typeCheckStmt st (Decl name rhs pos) = do
  ty <- inferExpr st rhs
  pure (addSymbol name ty pos st)

typeCheckStmt st (Assign name rhs pos) = do
  actualTy <- inferExpr st rhs
  -- The use of `case` to handle a `Maybe` value is idiomatic Haskell.
  -- It allows for clear and exhaustive handling of both `Nothing` and `Just`.
  case lookupSymbol name st of
    Nothing -> pure st -- Undefined variable already reported by ScopeResolution pass.
    Just info -> do
      let expectedTy = symbolType info
      -- Only report a mismatch if both types are known and different.
      let shouldReportError = expectedTy /= actualTy && expectedTy /= TUnknown && actualTy /= TUnknown
      when shouldReportError $
        logError (AssignmentTypeMismatch name expectedTy actualTy pos)
      pure st

typeCheckStmt st (ExprStmt e) = do
  _ <- inferExpr st e
  pure st

typeCheckStmt st (If cond thenBlock elseBlock ifPos) = do
  condTy <- inferExpr st cond
  -- Report an error if the condition is not a boolean, unless it's already an unknown type.
  when (condTy /= TBool && condTy /= TUnknown) $
    logError (InvalidCondition condTy (exprPosOr ifPos cond))

  -- Each branch gets its own scope; declarations inside it don't leak.
  _ <- typeCheckStmts (enterScope st) thenBlock
  case elseBlock of
    Nothing -> pure ()
    Just eb -> () <$ typeCheckStmts (enterScope st) eb
  pure st

typeCheckStmt st (OnEvent _event body _pos) = do
  _ <- typeCheckStmts (enterScope st) body
  pure st

-- | Infer the Type of an expression, logging any type errors found
-- along the way. Returns TUnknown wherever the "real" type can't be
-- determined to prevent cascading errors.
inferExpr :: SymbolTable -> Expr -> SemanticM Type
inferExpr _ (LitInt _)  = pure TInt
inferExpr _ (LitStr _)  = pure TString
inferExpr _ (LitBool _) = pure TBool
inferExpr st (Var name _pos) =
  case lookupSymbol name st of
    Just info -> pure (symbolType info)
    Nothing   -> pure TUnknown  -- Undefined; handled by ScopeResolution.
inferExpr st (Member baseExpr field _pos) = do
  _ <- inferExpr st baseExpr
  case baseName baseExpr of
    Just base | isBuiltinObject base ->
      case lookupMember base field of
        Just ty -> pure ty
        Nothing -> pure TUnknown  -- Unknown member; handled by ScopeResolution.
    _ -> pure TUnknown
inferExpr st (Call callee args pos) = do
  argTys   <- mapM (inferExpr st) args
  calleeTy <- inferExpr st callee
  case calleeTy of
    TFunction paramTys retTy -> do
      checkArgTypes paramTys argTys args pos (calleeLabel callee)
      pure retTy
    TUnknown -> pure TUnknown
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

-- | Type rules for binary operators.
inferBinary :: BinOp -> Type -> Type -> Position -> SemanticM Type
inferBinary _ TUnknown _ _ = pure TUnknown
inferBinary _ _ TUnknown _ = pure TUnknown
inferBinary op lty rty pos
  -- Arithmetic operators require two integer operands.
  | op `elem` [Add, Sub, Mul, Div, Mod] = do
      if lty == TInt && rty == TInt
        then pure TInt
        else do
          logError (InvalidOperandType (opSymbol op) (worseOperand lty rty) pos)
          pure TUnknown
  -- Equality operators require operands of the same type.
  | op `elem` [Eq, NotEq] = do
      when (lty /= rty) $
        logError (TypeMismatch lty rty "comparison" pos)
      -- Equality checks always yield a boolean, even if types mismatch,
      -- to prevent cascading "InvalidCondition" errors in `if` statements.
      pure TBool
  -- Relational operators require two integer operands.
  | op `elem` [Lt, Gt, Le, Ge] = do
      if lty == TInt && rty == TInt
        then pure TBool
        else do
          logError (InvalidOperandType (opSymbol op) (worseOperand lty rty) pos)
          -- Like equality, this yields a boolean to prevent cascade.
          pure TBool
  | otherwise = pure TUnknown

-- | Which side to blame when a binary operand type check fails.
worseOperand :: Type -> Type -> Type
worseOperand lty rty
  | lty /= TInt = lty
  | otherwise   = rty

-- | A string representation for a binary operator.
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
-- declared parameter types.
checkArgTypes :: [Type] -> [Type] -> [Expr] -> Position -> String -> SemanticM ()
checkArgTypes paramTys argTys argExprs pos label = do
  -- Arity check is handled by ScopeResolution, so we only check types if counts match.
  when (length paramTys == length argTys) $
    mapM_ checkOne (zip3 paramTys argTys argExprs)
  where
    checkOne (expected, actual, argExpr) = do
      -- Report an error if types mismatch, unless the actual type is unknown
      -- (which means an error was already reported deeper in the AST).
      let shouldReportError = expected /= actual && actual /= TUnknown
      when shouldReportError $
        logError (TypeMismatch expected actual ("argument to " ++ label) (exprPosOr pos argExpr))

-- | A human-readable name for a callee, for use in error messages.
calleeLabel :: Expr -> String
calleeLabel (Member baseExpr field _) = case baseName baseExpr of
  Just base -> base ++ "." ++ field
  Nothing   -> field
calleeLabel (Var name _) = name
calleeLabel _             = "<expression>"

-- | The immediate identifier a Member/Call chain is rooted on.
baseName :: Expr -> Maybe String
baseName (Var name _) = Just name
baseName _             = Nothing

-- | The position an expression itself carries, if any.
exprPos :: Expr -> Maybe Position
exprPos (LitInt _)      = Nothing
exprPos (LitStr _)      = Nothing
exprPos (LitBool _)     = Nothing
exprPos (Var _ p)       = Just p
exprPos (Member _ _ p)  = Just p
exprPos (Call _ _ p)    = Just p
exprPos (Binary _ _ _ p) = Just p
exprPos (Unary _ _ p)   = Just p

-- | exprPos with a fallback position for literals, which have none of their own.
exprPosOr :: Position -> Expr -> Position
exprPosOr fallback e = maybe fallback id (exprPos e)