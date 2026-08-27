module VM.VM
  ( HostEnv(..)
  , defaultHostEnv
  , VarEnv
  , VMState(..)
  , runEvent
  , execLoop
  , fetchInstr
  ) where

import qualified Data.Map as Map
import Data.Map (Map)
import Data.List (findIndex, sort)

import AST (Ident, BinOp(..), UnaryOp(..), EventSpec)
import Bytecode.Instruction (Instr(..), Label)
import VM.Stack (Value(..), Stack, emptyStack, push, pop)
import VM.EventTrigger (CompiledProgram(..), findHandler)

-- | Pluggable interface for anything that touches the outside world --
-- member reads (player.distance) and method calls (npc.say("hi")) -- so the
-- VM's core loop doesn't need to know Simulation/HostBindings exist.
data HostEnv = HostEnv
  { hostLoad :: Ident -> Ident -> IO Value
    -- ^ hostLoad "player" "distance" -> current value of player.distance
  , hostCall :: Ident -> Ident -> [Value] -> IO Value
    -- ^ hostCall "npc" "say" [VStr "hi"] -> perform the call, return a
    -- result value, or VUnit if the call doesn't produce one.
  }

-- | Logs everything, returns VInt 0 / VUnit. Lets you run and trace bytecode
-- today without a real HostBindings/Simulation.GridWorld implementation.
-- Swap this for a real HostEnv once those exist -- nothing else in this
-- module needs to change.
defaultHostEnv :: HostEnv
defaultHostEnv = HostEnv
  { hostLoad = \obj field -> do
      putStrLn $ "[host] LOAD " ++ obj ++ "." ++ field ++ " (default: 0)"
      pure (VInt 0)
  , hostCall = \obj method args -> do
      putStrLn $ "[host] CALL " ++ obj ++ "." ++ method ++ show args
      pure VUnit
  }

-- | Runtime variable store. Semantic.ScopeResolution has already checked
-- scoping/shadowing/duplicate decls statically, so the VM trusts the
-- program is valid and keeps one flat map rather than re-deriving block
-- scope at runtime.
--
-- Caveat: if a variable is shadowed inside an if/event block, the inner
-- STORE_VAR overwrites the outer variable's slot here rather than
-- restoring the outer value when the block exits. Unlikely to matter for
-- this project, but it's a real simplification, not an oversight.
type VarEnv = Map Ident Value

data VMState = VMState
  { vmStack  :: !Stack
  , vmVars   :: !VarEnv
  , vmPC     :: !Int      -- index into vmInstrs
  , vmInstrs :: [Instr]  -- the full flat instruction stream (whole program)
  } deriving (Show)

initVMState :: [Instr] -> VarEnv -> Int -> VMState
initVMState instrs vars startPC = VMState
  { vmStack  = emptyStack
  , vmVars   = vars
  , vmPC     = startPC
  , vmInstrs = instrs
  }

-- | Run one event to completion: find its handler's entry label, figure out
-- where that handler's code ends (the next handler's LABEL, or end of the
-- program), and execute from start to end. Returns the updated variable
-- store so the caller can feed it back in for the next event.
runEvent :: HostEnv -> CompiledProgram -> EventSpec -> VarEnv -> IO VarEnv
runEvent host prog spec vars =
  case findHandler spec prog of
    Nothing -> do
      putStrLn $ "[vm] no handler registered for event: " ++ show spec
      pure vars
    Just label ->
      case findLabelIndex label instrs of
        Nothing -> do
          putStrLn $ "[vm] internal error: entry label not found in bytecode: " ++ label
          pure vars
        Just startPC -> do
          let endPC = handlerEnd label instrs (map snd (cpHandlers prog))
              st0   = initVMState instrs vars startPC
          stFinal <- execLoop host endPC st0
          pure (vmVars stFinal)
  where
    instrs = cpInstrs prog

-- | Exclusive end-index for the handler starting at `label`: the index of
-- the nearest later handler-entry label, or the length of the instruction
-- stream if this is the last handler. This is what stops one handler's
-- execution from falling through into the next one's code.
handlerEnd :: Label -> [Instr] -> [Label] -> Int
handlerEnd label instrs allHandlerLabels =
  case sort laterStarts of
    (next:_) -> next
    []       -> length instrs
  where
    startIdx = maybe (length instrs) id (findLabelIndex label instrs)
    laterStarts =
      [ i | l <- allHandlerLabels
          , l /= label
          , Just i <- [findLabelIndex l instrs]
          , i > startIdx
      ]

findLabelIndex :: Label -> [Instr] -> Maybe Int
findLabelIndex lbl = findIndex isLbl
  where
    isLbl (LABEL l) = l == lbl
    isLbl _         = False

advance :: VMState -> VMState
advance s = s { vmPC = vmPC s + 1 }
-- | Safe fetch: turns an out-of-range PC into a descriptive VM error
-- instead of a bare Prelude.!! crash. Should be unreachable given
-- runEvent's endPC bound and well-formed jump targets, but a corrupt or
-- negative PC (e.g. a bug in handlerEnd, or a jump to a label whose index
-- lands outside the handler's own range) shouldn't produce a cryptic
-- "index too large" message.
fetchInstr :: VMState -> Instr
fetchInstr st
  | pc < 0 || pc >= length is = error $
      "[vm] internal error: program counter out of bounds: " ++ show pc ++
      " (instruction stream has " ++ show (length is) ++ " instructions)"
  | otherwise = is !! pc
  where
    pc = vmPC st
    is = vmInstrs st

-- | Fetch-execute loop, bounded to [.., endPC). Stops as soon as the PC
-- reaches endPC -- it never needs to inspect instructions to know when to
-- stop, since runEvent already computed the boundary.
execLoop :: HostEnv -> Int -> VMState -> IO VMState
execLoop host endPC st
  | vmPC st >= endPC = pure st
  | otherwise = do
      let instr = vmInstrs st !! vmPC st
      st' <- step host st instr
      execLoop host endPC st'

step :: HostEnv -> VMState -> Instr -> IO VMState
step host st instr = case instr of

  PUSH_INT n  -> pure $ advance st { vmStack = push (VInt n) (vmStack st) }
  PUSH_STR s  -> pure $ advance st { vmStack = push (VStr s) (vmStack st) }
  PUSH_BOOL b -> pure $ advance st { vmStack = push (VBool b) (vmStack st) }

  LOAD obj field -> do
    v <- hostLoad host obj field
    pure $ advance st { vmStack = push v (vmStack st) }

  LOAD_VAR name ->
    case Map.lookup name (vmVars st) of
      Just v  -> pure $ advance st { vmStack = push v (vmStack st) }
      Nothing -> error $
        "[vm] undefined variable at runtime: " ++ name ++
        " (Semantic.ScopeResolution should have caught this at compile time)"

  STORE_VAR name ->
    let (v, stack') = pop (vmStack st)
    in pure $ advance st { vmStack = stack', vmVars = Map.insert name v (vmVars st) }

  BINOP op ->
    let (rhs, s1) = pop (vmStack st)
        (lhs, s2) = pop s1
    in pure $ advance st { vmStack = push (evalBinOp op lhs rhs) s2 }

  UNOP op ->
    let (v, s1) = pop (vmStack st)
    in pure $ advance st { vmStack = push (evalUnaryOp op v) s1 }

  JUMP_IF_FALSE label ->
    let (v, s1) = pop (vmStack st)
    in case v of
         VBool False -> jumpTo label st { vmStack = s1 }
         VBool True  -> pure $ advance st { vmStack = s1 }
         other       -> error $ "[vm] JUMP_IF_FALSE expects a bool on the stack, got: " ++ show other

  JUMP label -> jumpTo label st

  LABEL _ -> pure $ advance st  -- pure marker, no-op at runtime

  CALL objName method argc -> do
    let (args, stack') = popN argc (vmStack st)
    result <- hostCall host objName method args
    pure $ advance st { vmStack = push result stack' }

  POP ->
    let (_, s1) = pop (vmStack st)
    in pure $ advance st { vmStack = s1 }
 
  where
    jumpTo label s = case findLabelIndex label (vmInstrs s) of
      Just idx -> pure s { vmPC = idx }
      Nothing  -> error $ "[vm] jump to undefined label: " ++ label

    -- Pops argc values and returns them in original left-to-right call
    -- order (args are pushed arg1..argN, so the first thing popped is
    -- argN; prepending each pop as we go reconstructs arg1..argN).
    popN n stack = go n stack []
      where
        go 0 s acc = (acc, s)
        go k s acc = let (v, s') = pop s in go (k - 1 :: Int) s' (v : acc)

evalBinOp :: BinOp -> Value -> Value -> Value
evalBinOp Add (VInt a) (VInt b) = VInt (a + b)
evalBinOp Add (VStr a) (VStr b) = VStr (a ++ b)
evalBinOp Sub (VInt a) (VInt b) = VInt (a - b)
evalBinOp Mul (VInt a) (VInt b) = VInt (a * b)
evalBinOp Div (VInt a) (VInt b) = VInt (a `div` b)
evalBinOp Mod (VInt a) (VInt b) = VInt (a `mod` b)
evalBinOp Eq    a b = VBool (a == b)
evalBinOp NotEq a b = VBool (a /= b)
evalBinOp Lt (VInt a) (VInt b) = VBool (a < b)
evalBinOp Gt (VInt a) (VInt b) = VBool (a > b)
evalBinOp Le (VInt a) (VInt b) = VBool (a <= b)
evalBinOp Ge (VInt a) (VInt b) = VBool (a >= b)
evalBinOp op a b = error $
  "[vm] type error: cannot apply " ++ show op ++ " to " ++ show a ++ " and " ++ show b

evalUnaryOp :: UnaryOp -> Value -> Value
evalUnaryOp Neg (VInt n)  = VInt (negate n)
evalUnaryOp Not (VBool b) = VBool (not b)
evalUnaryOp op v = error $
  "[vm] type error: cannot apply " ++ show op ++ " to " ++ show v