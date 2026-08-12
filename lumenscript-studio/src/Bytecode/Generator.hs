module Bytecode.Generator
  ( generateBytecode
  , GeneratorState(..)
  , initialState
  , BytecodeError(..)
  ) where

import qualified Data.Map as Map
import Control.Monad (foldM)

import AST
import Bytecode.Instruction hiding (handlers)
import qualified Bytecode.SymbolTable as ST

-- | Errors that can occur during bytecode generation.
data BytecodeError
  = UnboundVariable String
  deriving (Eq, Show)

-- | The state of the bytecode generator.
data GeneratorState = GeneratorState
  { instructions :: [Instr]
  , labelCount   :: Int
  , symbolTable  :: ST.SymbolTable
  , handlers     :: Map.Map Label [Instr]
  } deriving (Show, Eq)

-- | The initial state for the generator.
initialState :: GeneratorState
initialState = GeneratorState
  { instructions = []
  , labelCount   = 0
  , symbolTable  = ST.empty
  , handlers     = Map.empty
  }

-- | The main entry point for bytecode generation.
generateBytecode :: Program -> Either BytecodeError CompiledProgram
generateBytecode stmts =
  let initialState' = initialState { symbolTable = ST.fromProgram stmts }
  in case generateStmts initialState' stmts of
    Left err -> Left err
    Right finalState ->
      let main = reverse (instructions finalState)
          handlersList = Map.toList (handlers finalState)
      in Right (CompiledProgram main handlersList)

-- | Generate instructions for a list of statements.
generateStmts :: GeneratorState -> [Stmt] -> Either BytecodeError GeneratorState
generateStmts = foldM generateStmt

-- | Generate instructions for a single statement.
generateStmt :: GeneratorState -> Stmt -> Either BytecodeError GeneratorState
generateStmt st (ExprStmt expr) = do
  (st', exprInstrs) <- generateExpr st expr
  pure $ st' { instructions = POP : exprInstrs ++ instructions st' }

generateStmt st (Decl name expr _) = do
  (st', exprInstrs) <- generateExpr st expr
  let (newSymbolTable, newIndex) =
        if ST.isGlobal (symbolTable st')
        then case ST.resolve name (symbolTable st') of
               Just idx -> (symbolTable st', idx)
               Nothing  -> ST.define name (symbolTable st') -- Should not happen
        else ST.define name (symbolTable st')
      newInstructions = STORE_LOCAL newIndex : exprInstrs
  pure $ st'
    { instructions = newInstructions ++ instructions st'
    , symbolTable  = newSymbolTable
    }

generateStmt st (Assign lhs expr _) = do
  (st', exprInstrs) <- generateExpr st expr
  case lhs of
    Var name _ ->
      case ST.resolve name (symbolTable st') of
        Just index ->
          let newInstructions = STORE_LOCAL index : exprInstrs
          in Right $ st' { instructions = newInstructions ++ instructions st' }
        Nothing -> Left (UnboundVariable name)
    Member obj field _ -> do
        (st'', objInstrs) <- generateExpr st' obj
        let newInstructions = STORE_MEMBER field : exprInstrs ++ objInstrs
        Right $ st'' { instructions = newInstructions ++ instructions st'' }
    -- isLValue check in semantic analysis should prevent other cases.
    _ -> Right st'

generateStmt st (If cond thenBlock mElseBlock _) = do
  (stAfterCond, condInstrs) <- generateExpr st cond

  case mElseBlock of
    Nothing -> do
      let (stWithEndLabel, endLabel) = newLabel stAfterCond "end_if"
      let stForThen = stWithEndLabel { instructions = [], symbolTable = ST.enterScope (symbolTable stWithEndLabel) }
      stAfterThenBody <- generateStmts stForThen thenBlock
      let thenInstrs = instructions stAfterThenBody
      let stAfterThen = stAfterThenBody { instructions = instructions st, symbolTable = ST.exitScope (symbolTable stAfterThenBody) }
      let finalInstructions =
            [LABEL endLabel] ++
            thenInstrs ++
            [JUMP_IF_FALSE endLabel] ++
            condInstrs
      pure $ stAfterThen { instructions = finalInstructions ++ instructions st }

    Just elseBlock' -> do
      let (stWithElseLabel, elseLabel) = newLabel stAfterCond "else"
      let (stWithEndLabel, endLabel) = newLabel stWithElseLabel "end_if"

      let stForThen = stWithEndLabel { instructions = [], symbolTable = ST.enterScope (symbolTable stWithEndLabel) }
      stAfterThenBody <- generateStmts stForThen thenBlock
      let thenInstrs = instructions stAfterThenBody

      let stForElse = stWithEndLabel { instructions = [], symbolTable = ST.enterScope (symbolTable stWithEndLabel), labelCount = labelCount stAfterThenBody }
      stAfterElseBody <- generateStmts stForElse elseBlock'
      let elseInstrs = instructions stAfterElseBody

      let stAfterElse = stAfterElseBody { instructions = instructions st, symbolTable = ST.exitScope (symbolTable stAfterElseBody) }

      let finalInstructions =
            [LABEL endLabel] ++
            elseInstrs ++
            [LABEL elseLabel] ++
            [JUMP endLabel] ++
            thenInstrs ++
            [JUMP_IF_FALSE elseLabel] ++
            condInstrs
      pure $ stAfterElse { instructions = finalInstructions ++ instructions st }

generateStmt st (OnEvent (KeyPress eventType) body _) = do
  let handlerLabel = "key_press_" ++ eventType
  let stForHandler = initialState { labelCount = labelCount st, symbolTable = symbolTable st }
  stAfterHandler <- generateStmts stForHandler body
  pure $ st
    { handlers = Map.insert handlerLabel (reverse (instructions stAfterHandler)) (handlers st)
    , labelCount = labelCount stAfterHandler
    }

-- | Generate instructions for an expression.
generateExpr :: GeneratorState -> Expr -> Either BytecodeError (GeneratorState, [Instr])
generateExpr st (LitInt n)  = Right (st, [PUSH_INT n])
generateExpr st (LitStr s)  = Right (st, [PUSH_STR s])
generateExpr st (LitBool b) = Right (st, [PUSH_BOOL b])

generateExpr st (Var name _) =
  case ST.resolve name (symbolTable st) of
    Just index -> Right (st, [LOAD_LOCAL index])
    Nothing    -> Right (st, [LOAD name]) -- Fallback for built-ins

generateExpr st (Unary op expr _) = do
  (st', exprInstrs) <- generateExpr st expr
  Right (st', UOP op : exprInstrs)

generateExpr st (Binary op left right _) = do
  (st', leftInstrs) <- generateExpr st left
  (st'', rightInstrs) <- generateExpr st' right
  Right (st'', OP op : rightInstrs ++ leftInstrs)

generateExpr st (Call callee args _) = do
  (stAfterArgs, argsInstrs) <- generateExprs st args
  (stAfterCallee, calleeInstrs) <- generateExpr stAfterArgs callee
  let callInstr = CALL (calleeLabel callee) (length args)
  Right (stAfterCallee, callInstr : calleeInstrs ++ argsInstrs)

generateExpr st (Member obj field _) = do
    (st', objInstrs) <- generateExpr st obj
    Right (st', LOAD_MEMBER field : objInstrs)

-- | Generate instructions for a list of expressions.
generateExprs :: GeneratorState -> [Expr] -> Either BytecodeError (GeneratorState, [Instr])
-- We want to evaluate arguments from left-to-right. Since we are building a
-- reversed list of instructions, we should process the argument expressions
-- from left-to-right, prepending the generated instructions each time.
generateExprs st exprs = foldM (\(s, acc) e -> do (s', i) <- generateExpr s e; Right (s', i ++ acc)) (st, []) exprs

-- | Create a new, unique label.
newLabel :: GeneratorState -> String -> (GeneratorState, Label)
newLabel st prefix =
  let count = labelCount st
      label = prefix ++ "_" ++ show count
  in (st { labelCount = count + 1 }, label)

-- | Get a string representation of a callee for use in CALL instructions.
calleeLabel :: Expr -> String
calleeLabel (Var name _) = name
calleeLabel (Member obj field _) = calleeLabel obj ++ "." ++ field
calleeLabel other = show other