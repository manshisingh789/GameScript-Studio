module Bytecode.Generator where

import AST
import Bytecode.Instruction
import Control.Monad.State
import qualified Data.Map as Map

-- A placeholder for the actual SemanticError type
data SemanticError = DummyError String deriving (Show, Eq)

type Generator a = StateT GeneratorState (Either SemanticError) a

data GeneratorState = GeneratorState
  { instructions :: [Instr]
  , labelCount :: Int
  , handlerMap :: Map.Map Label [Instr]
  }

initialState :: GeneratorState
initialState = GeneratorState { instructions = [], labelCount = 0, handlerMap = Map.empty }

-- Main public function
generateBytecode :: Program -> Either SemanticError CompiledProgram
generateBytecode stmts =
  case execStateT (generateBlock stmts) initialState of
    Left err -> Left err
    Right finalState ->
      let
        main = reverse $ instructions finalState
        handlersList = Map.toList $ handlerMap finalState
      in
      Right $ CompiledProgram { mainBlock = main, handlers = handlersList }

-- Internal functions
generateStmt :: Stmt -> Generator ()
generateStmt (ExprStmt expr) = generateExpr expr
generateStmt (If condition thenBlock mElseBlock) =
  case mElseBlock of
    Nothing -> do
      endLabel <- newLabel
      generateExpr condition
      emit $ JUMP_IF_FALSE endLabel
      generateBlock thenBlock
      emit $ LABEL endLabel
    Just elseBlock -> do
      elseLabel <- newLabel
      endLabel <- newLabel
      generateExpr condition
      emit $ JUMP_IF_FALSE elseLabel
      generateBlock thenBlock
      emit $ JUMP endLabel
      emit $ LABEL elseLabel
      generateBlock elseBlock
      emit $ LABEL endLabel
generateStmt (OnEvent (KeyPress eventType) body) = do
    currentState <- get
    let handlerInitialState = GeneratorState { instructions = [], labelCount = labelCount currentState, handlerMap = Map.empty }

    case execStateT (generateBlock body) handlerInitialState of
        Left err -> lift (Left err)
        Right handlerFinalState -> do
            let handlerInstrs = reverse $ instructions handlerFinalState
            let handlerLabel = "key_press_" ++ eventType
            put $ currentState
                { labelCount = labelCount handlerFinalState
                , handlerMap = Map.insert handlerLabel handlerInstrs (handlerMap currentState)
                }
generateStmt (Decl var expr) = do
  generateExpr expr
  emit $ STORE var
generateStmt (Assign var expr) = do
  generateExpr expr
  emit $ STORE var
generateStmt _ = undefined -- Handle other statements later

generateExpr :: Expr -> Generator ()
generateExpr (LitInt n) = emit $ PUSH_INT n
generateExpr (LitStr s) = emit $ PUSH_STR s
generateExpr (LitBool b) = emit $ PUSH_BOOL b
generateExpr (Var ident) = emit $ LOAD ident
generateExpr (Member obj member) = do
  generateExpr obj
  emit $ LOAD_MEMBER member
generateExpr (Unary op expr) = do
  generateExpr expr
  emit $ UOP op
generateExpr (Binary op left right) = do
  generateExpr left
  generateExpr right
  emit $ OP op
generateExpr (Call callee args) = do
  mapM_ generateExpr args
  generateExpr callee
  emit $ CALL (show callee) (length args)
generateExpr _ = undefined -- Handle other expressions later

generateBlock :: [Stmt] -> Generator ()
generateBlock = mapM_ generateStmt

newLabel :: Generator Label
newLabel = do
  count <- gets labelCount
  modify $ \s -> s { labelCount = count + 1 }
  return $ "L" ++ show count

emit :: Instr -> Generator ()
emit instr = modify $ \s -> s { instructions = instr : instructions s }