module VM.Stack
  ( Value(..)
  , Stack
  , emptyStack
  , push
  , pop
  , peek
  , isEmpty
  ) where

-- | Runtime values the VM pushes/pops. VUnit is what a CALL produces when
-- there's nothing meaningful to return (see HostEnv.hostCall in VM.hs) --
-- it exists so that ExprStmt's trailing POP always has something to discard.
data Value
  = VInt Int
  | VStr String
  | VBool Bool
  | VUnit
  deriving (Eq)

instance Show Value where
  show (VInt n)  = show n
  show (VStr s)  = show s
  show (VBool b) = show b
  show VUnit     = "()"

-- | The VM's operand stack. A newtype over a list (head = top of stack) so
-- VM.hs only ever touches it through push/pop/peek/isEmpty, never list ops
-- directly.
newtype Stack = Stack [Value]

instance Show Stack where
  show (Stack vs) = "Stack " ++ show vs

emptyStack :: Stack
emptyStack = Stack []

push :: Value -> Stack -> Stack
push v (Stack vs) = Stack (v : vs)

-- | Pop the top value off the stack. Errors on empty stack -- an empty pop
-- means the compiler emitted unbalanced bytecode (a codegen bug), not a
-- condition the VM should try to recover from at runtime.
pop :: Stack -> (Value, Stack)
pop (Stack [])     = error "[vm] stack underflow: pop on empty stack"
pop (Stack (v:vs)) = (v, Stack vs)

peek :: Stack -> Value
peek (Stack [])    = error "[vm] stack underflow: peek on empty stack"
peek (Stack (v:_)) = v

isEmpty :: Stack -> Bool
isEmpty (Stack vs) = null vs