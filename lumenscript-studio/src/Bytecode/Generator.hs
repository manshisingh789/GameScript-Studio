
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
import VM.EventTrigger (CompiledProgram(..))


-- | Errors that can occur during bytecode generation.
data BytecodeError
  = UnboundVariable String
  deriving (Eq, Show)


-- | The state of the bytecode generator.
data GeneratorState = GeneratorState
  { instructions :: [Instr]
  , labelCount   :: Int
  , symbolTable  :: ST.SymbolTable
  , handlers     :: [(EventSpec, Label)]
  } deriving (Show, Eq)


-- | The initial state for the generator.
initialState :: GeneratorState
initialState = GeneratorState
  { instructions = []
  , labelCount   = 0
  , symbolTable  = ST.empty
  , handlers     = []
  }


-- | The main entry point for bytecode generation.
generateBytecode :: Program -> Either BytecodeError CompiledProgram
generateBytecode stmts =
  let initialState' =
        initialState
          { symbolTable = ST.fromProgram stmts
          }

  in case generateStmts initialState' stmts of
       Left err ->
         Left err

       Right finalState ->
         let mainInstructions =
               reverse (instructions finalState)

             handlersList =
               handlers finalState

         in Right
              (CompiledProgram
                mainInstructions
                handlersList
              )


-- | Generate instructions for a list of statements.
generateStmts
  :: GeneratorState
  -> [Stmt]
  -> Either BytecodeError GeneratorState
generateStmts =
  foldM generateStmt


-- | Generate instructions for a single statement.
generateStmt
  :: GeneratorState
  -> Stmt
  -> Either BytecodeError GeneratorState


-- | Generate an expression statement.
generateStmt st (ExprStmt expr) = do
  (st', exprInstrs) <-
    generateExpr st expr

  pure $
    st'
      { instructions =
          POP : exprInstrs ++ instructions st'
      }


-- | Generate a variable declaration.
generateStmt st (Decl name expr _) = do
  (st', exprInstrs) <-
    generateExpr st expr

  let newInstructions =
        STORE_VAR name : exprInstrs

  pure $
    st'
      { instructions =
          newInstructions ++ instructions st'
      }


-- | Generate an assignment.
generateStmt st (Assign lhs expr _) = do
  (st', exprInstrs) <-
    generateExpr st expr

  case lhs of

    -- x = expression
    Var name _ ->
      case ST.resolve name (symbolTable st') of

        Just _ ->
          let newInstructions =
                STORE_VAR name : exprInstrs

          in Right $
               st'
                 { instructions =
                     newInstructions ++ instructions st'
                 }

        Nothing ->
          Left (UnboundVariable name)

    -- Member assignment is not currently supported by Instr.
    Member _ _ _ ->
      Right st'

    -- Invalid assignment target.
    _ ->
      Right st'


-- | Generate an if statement.
generateStmt st (If cond thenBlock mElseBlock _) = do

  (stAfterCond, condInstrs) <-
    generateExpr st cond

  case mElseBlock of

    -- if (condition) { ... }
    Nothing -> do

      let
        (stWithEndLabel, endLabel) =
          newLabel stAfterCond "end_if"

        stForThen =
          stWithEndLabel
            { instructions = []
            , symbolTable =
                ST.enterScope
                  (symbolTable stWithEndLabel)
            }

      stAfterThenBody <-
        generateStmts stForThen thenBlock

      let
        thenInstrs =
          instructions stAfterThenBody

        stAfterThen =
          stAfterThenBody
            { instructions = instructions st
            , symbolTable =
                ST.exitScope
                  (symbolTable stAfterThenBody)
            }

        finalInstructions =
          [ LABEL endLabel
          ]
          ++ thenInstrs
          ++
          [ JUMP_IF_FALSE endLabel
          ]
          ++ condInstrs

      pure $
        stAfterThen
          { instructions =
              finalInstructions ++ instructions st
          }


    -- if (condition) { ... } else { ... }
    Just elseBlock' -> do

      let
        (stWithElseLabel, elseLabel) =
          newLabel stAfterCond "else"

        (stWithEndLabel, endLabel) =
          newLabel stWithElseLabel "end_if"

        stForThen =
          stWithEndLabel
            { instructions = []
            , symbolTable =
                ST.enterScope
                  (symbolTable stWithEndLabel)
            }

      stAfterThenBody <-
        generateStmts stForThen thenBlock

      let
        thenInstrs =
          instructions stAfterThenBody

        stForElse =
          stWithEndLabel
            { instructions = []
            , symbolTable =
                ST.enterScope
                  (symbolTable stWithEndLabel)
            , labelCount =
                labelCount stAfterThenBody
            }

      stAfterElseBody <-
        generateStmts stForElse elseBlock'

      let
        elseInstrs =
          instructions stAfterElseBody

        stAfterElse =
          stAfterElseBody
            { instructions = instructions st
            , symbolTable =
                ST.exitScope
                  (symbolTable stAfterElseBody)
            }

        finalInstructions =
          [ LABEL endLabel
          ]
          ++ elseInstrs
          ++
          [ LABEL elseLabel
          , JUMP endLabel
          ]
          ++ thenInstrs
          ++
          [ JUMP_IF_FALSE elseLabel
          ]
          ++ condInstrs

      pure $
        stAfterElse
          { instructions =
              finalInstructions ++ instructions st
          }


-- | Generate a key-press event handler.
generateStmt
  st
  (OnEvent (KeyPress eventType) body _) = do

  let
    handlerLabel =
      "key_press_" ++ eventType

    eventSpec =
      KeyPress eventType

    stForHandler =
      initialState
        { labelCount =
            labelCount st
        , symbolTable =
            symbolTable st
        }

  stAfterHandler <-
    generateStmts stForHandler body

  let handlerInstructions =
        reverse (instructions stAfterHandler)

  pure $
    st
      { handlers =
          (eventSpec, handlerLabel)
          : handlers st

      , instructions =
          instructions st
          ++ [LABEL handlerLabel]
          ++ handlerInstructions

      , labelCount =
          labelCount stAfterHandler
      }


-- | Generate instructions for an expression.
generateExpr
  :: GeneratorState
  -> Expr
  -> Either BytecodeError (GeneratorState, [Instr])


-- Integer literal
generateExpr st (LitInt n) =
  Right
    (st, [PUSH_INT n])


-- String literal
generateExpr st (LitStr s) =
  Right
    (st, [PUSH_STR s])


-- Boolean literal
generateExpr st (LitBool b) =
  Right
    (st, [PUSH_BOOL b])


-- Variable
generateExpr st (Var name _) =
  case ST.resolve name (symbolTable st) of

    Just _ ->
      Right
        (st, [LOAD_VAR name])

    Nothing ->
      Right
        (st, [LOAD name ""])


-- Unary expression
generateExpr st (Unary op expr _) = do

  (st', exprInstrs) <-
    generateExpr st expr

  Right
    (st', UNOP op : exprInstrs)


-- Binary expression
generateExpr st (Binary op left right _) = do

  (st', leftInstrs) <-
    generateExpr st left

  (st'', rightInstrs) <-
    generateExpr st' right

  Right
    ( st''
    , BINOP op
        : rightInstrs
        ++ leftInstrs
    )


-- Function / method call
generateExpr st (Call callee args _) = do

  (stAfterArgs, argsInstrs) <-
    generateExprs st args

  case callee of

    -- object.method(...)
    Member obj field _ ->

      case obj of

        Var objectName _ ->

          let callInstr =
                CALL
                  objectName
                  field
                  (length args)

          in Right
               ( stAfterArgs
               , callInstr : argsInstrs
               )

        _ ->
          Left
            (UnboundVariable
              (calleeLabel obj))


    -- Plain function call.
    Var functionName _ ->

      let callInstr =
            CALL
              ""
              functionName
              (length args)

      in Right
           ( stAfterArgs
           , callInstr : argsInstrs
           )

    _ ->
      Left
        (UnboundVariable
          (calleeLabel callee))


-- | Member access.
--
-- Example:
--
-- player.distance
--
-- becomes:
--
-- LOAD player distance
generateExpr
  st
  (Member (Var objectName _) field _) =

  Right
    (st, [LOAD objectName field])


-- | More complicated member expressions.
generateExpr
  st
  (Member obj field _) = do

  (st', objInstrs) <-
    generateExpr st obj

  Right
    (st', objInstrs ++ [LOAD (calleeLabel obj) field])


-- | Generate instructions for a list of expressions.
--
-- Arguments are evaluated from left to right.
generateExprs
  :: GeneratorState
  -> [Expr]
  -> Either BytecodeError (GeneratorState, [Instr])

generateExprs st exprs =
  foldM
    (\(s, acc) e -> do

       (s', i) <-
         generateExpr s e

       Right
         (s', i ++ acc)

    )
    (st, [])
    exprs


-- | Create a new, unique label.
newLabel
  :: GeneratorState
  -> String
  -> (GeneratorState, Label)

newLabel st prefix =
  let
    count =
      labelCount st

    label =
      prefix ++ "_" ++ show count

  in
    ( st
        { labelCount =
            count + 1
        }
    , label
    )


-- | Get a string representation of a callee.
calleeLabel :: Expr -> String

calleeLabel (Var name _) =
  name

calleeLabel (Member obj field _) =
  calleeLabel obj ++ "." ++ field

calleeLabel other =
  show other

