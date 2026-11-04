module Statistics.CompilationStats
    ( CompilationStats(..)
    , countLogicalLines
    , countTokens
    , countASTNodes
    , countBytecodeInstructions
    ) where

import Data.Char (isSpace)
import AST
import Token
import Bytecode.Instruction
import VM.EventTrigger (CompiledProgram(..))

data CompilationStats = CompilationStats
    { statLogicalLines       :: Int
    , statTokenCount         :: Int
    , statASTNodes           :: Maybe Int
    , statBytecodeCount      :: Maybe Int
    , statCompileTimeMs      :: Double
    } deriving (Show, Eq)


-- Count non-empty, non-comment-only source lines.
countLogicalLines :: String -> Int
countLogicalLines source =
    length $ filter isCodeLine (lines source)
  where
    isCodeLine lineText =
        let trimmed = dropWhile isSpace lineText
        in not (null trimmed) && head trimmed /= '#'


-- Count all tokens, including EOF.
countTokens :: [Token] -> Int
countTokens = length


-- Count AST nodes recursively.
-- Every Stmt, Expr, and EventSpec constructor counts as one node.
countASTNodes :: Program -> Int
countASTNodes = sum . map countStmt
  where
    countStmt :: Stmt -> Int
    countStmt stmt = case stmt of
        Decl _ _ expr _ ->
            1 + countExpr expr

        Assign target value _ ->
            1 + countExpr target + countExpr value

        ExprStmt expr ->
            1 + countExpr expr

        If condition thenBody elseBody _ ->
            1 + countExpr condition
              + sum (map countStmt thenBody)
              + maybe 0 (sum . map countStmt) elseBody

        OnEvent eventSpec body _ ->
            1 + countEventSpec eventSpec
              + sum (map countStmt body)

    countExpr :: Expr -> Int
    countExpr expr = case expr of
        LitInt _ -> 1
        LitStr _ -> 1
        LitBool _ -> 1
        Var _ _ -> 1

        Member object _ _ ->
            1 + countExpr object

        Unary _ operand _ ->
            1 + countExpr operand

        Binary _ left right _ ->
            1 + countExpr left + countExpr right

        Call callee args _ ->
            1 + countExpr callee + sum (map countExpr args)

    countEventSpec :: EventSpec -> Int
    countEventSpec eventSpec = case eventSpec of
        KeyPress _ -> 1
        GenericEvent _ _ -> 1


-- Count executable bytecode instructions.
-- LABEL is metadata and is deliberately excluded.
countBytecodeInstructions :: CompiledProgram -> Int
countBytecodeInstructions =
    length . filter isExecutable . cpInstrs
  where
    isExecutable instr = case instr of
        LABEL _ -> False
        _       -> True