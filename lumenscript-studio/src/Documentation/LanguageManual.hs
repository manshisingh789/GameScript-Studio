module Documentation.LanguageManual
  ( ManualSection(..)
  , manualSections
  ) where

import Data.List (intercalate)

import ApiCatalog

data ManualSection = ManualSection
  { sectionId :: String
  , sectionTitle :: String
  , sectionContent :: String
  } deriving (Show, Eq)


manualSections :: [ManualSection]
manualSections =
  [ introduction
  , gettingStarted
  , variables
  , dataTypes
  , operators
  , conditionals
  , eventHandling
  , functions
  , engineApi
  , comments
  , examples
  , limitations
  , troubleshooting
  ]


introduction :: ManualSection
introduction = ManualSection
  "introduction"
  "Introduction"
  (unlines
    [ "Welcome to LumenScript!"
    , ""
    , "LumenScript is a lightweight scripting language designed for"
    , "game-related scripting and interactive simulations."
    , ""
    , "It allows you to:"
    , "  - Declare and modify variables."
    , "  - Perform arithmetic and comparisons."
    , "  - Control execution using conditional statements."
    , "  - Respond to keyboard events."
    , "  - Interact with game objects through engine API calls."
    , ""
    , "LumenScript Studio provides an environment for writing,"
    , "compiling, inspecting, and executing LumenScript programs."
    , ""
    , "A typical program passes through these stages:"
    , ""
    , "  Source Code -> Lexer -> Parser -> Semantic Analysis"
    , "             -> Bytecode Generation -> Virtual Machine"
    ])


gettingStarted :: ManualSection
gettingStarted = ManualSection
  "getting-started"
  "Getting Started"
  (unlines
    [ "Writing Your First Program"
    , ""
    , "Start by declaring a variable:"
    , ""
    , "  let score = 10"
    , ""
    , "You can display a message using the print function:"
    , ""
    , "  print(\"Hello, LumenScript!\")"
    , ""
    , "Your complete first program:"
    , ""
    , "  let score = 10"
    , "  print(\"Hello, LumenScript!\")"
    , ""
    , "Using LumenScript Studio:"
    , ""
    , "  1. Enter your program in the editor."
    , "  2. Click Compile."
    , "  3. Inspect the generated tokens, AST, and bytecode."
    , "  4. Use Step to execute one VM instruction."
    , "  5. Use Run to execute continuously."
    , "  6. Use Pause to stop execution."
    , "  7. Use Reset to restore the initial VM state."
    ])


variables :: ManualSection
variables = ManualSection
  "variables"
  "Variables"
  (unlines
    [ "Variable declarations store values that can be used later."
    , ""
    , "Mutable Variables: let"
    , ""
    , "  let score = 10"
    , "  score = 20"
    , ""
    , "Variables declared with let can be reassigned."
    , ""
    , "Immutable Variables: const"
    , ""
    , "  const maxHealth = 100"
    , ""
    , "Variables declared with const cannot be reassigned."
    , "Attempting to modify one produces a semantic error."
    , ""
    , "Declaration Syntax"
    , ""
    , "  let identifier = expression"
    , "  const identifier = expression"
    , ""
    , "A declaration requires an identifier and an initial value."
    ])


dataTypes :: ManualSection
dataTypes = ManualSection
  "data-types"
  "Data Types and Literals"
  (unlines
    [ "LumenScript currently supports three literal types."
    , ""
    , "1. Integer"
    , ""
    , "  let lives = 3"
    , "  let score = 125"
    , ""
    , "Integer literals represent whole numbers."
    , ""
    , "2. String"
    , ""
    , "  let message = \"Welcome!\""
    , ""
    , "Strings are enclosed in double quotation marks."
    , ""
    , "Supported escape sequences:"
    , "  \\\"  Double quotation mark"
    , "  \\\\  Backslash"
    , "  \\n  Newline"
    , "  \\t  Tab"
    , ""
    , "3. Boolean"
    , ""
    , "  let isAlive = true"
    , "  let isPaused = false"
    , ""
    , "Boolean values are either true or false."
    ])


operators :: ManualSection
operators = ManualSection
  "operators"
  "Operators and Expressions"
  (unlines
    [ "Arithmetic Operators"
    , ""
    , "  +   Addition"
    , "  -   Subtraction"
    , "  *   Multiplication"
    , "  /   Integer division"
    , "  %   Modulo"
    , ""
    , "Example:"
    , ""
    , "  let total = 10 + 5"
    , "  let remaining = total - 3"
    , ""
    , "Comparison Operators"
    , ""
    , "  ==  Equal to"
    , "  !=  Not equal to"
    , "  <   Less than"
    , "  >   Greater than"
    , "  <=  Less than or equal to"
    , "  >=  Greater than or equal to"
    , ""
    , "Example:"
    , ""
    , "  let canAttack = health > 0"
    , ""
    , "Unary Operators"
    , ""
    , "  -   Numeric negation"
    , "  !   Boolean negation"
    , ""
    , "Example:"
    , ""
    , "  let negativeScore = -score"
    , "  let notReady = !ready"
    , ""
    , "Operator Precedence"
    , ""
    , "Expressions are parsed in the following order:"
    , "  1. Parenthesized expressions and primary expressions."
    , "  2. Unary operators."
    , "  3. Multiplication, division, and modulo."
    , "  4. Addition and subtraction."
    , "  5. Comparison operators."
    , ""
    , "Note: Logical operators && and || are not currently supported."
    ])


conditionals :: ManualSection
conditionals = ManualSection
  "conditionals"
  "Conditional Statements"
  (unlines
    [ "Conditional statements allow a program to execute different"
    , "blocks depending on the result of an expression."
    , ""
    , "If Statement"
    , ""
    , "  if score > 10: {"
    , "      print(\"High score!\")"
    , "  }"
    , ""
    , "If-Else Statement"
    , ""
    , "  if health > 0: {"
    , "      print(\"Player is alive.\")"
    , "  } else: {"
    , "      print(\"Game over.\")"
    , "  }"
    , ""
    , "Syntax Rules"
    , ""
    , "  - The condition follows the if keyword."
    , "  - A colon separates the condition from its block."
    , "  - Blocks are enclosed in curly braces."
    , "  - else is optional."
    ])


eventHandling :: ManualSection
eventHandling = ManualSection
  "events"
  "Event Handling"
  (unlines
    [ "LumenScript supports keyboard event handlers."
    , ""
    , "A keyboard handler executes when its registered key is pressed."
    , ""
    , "Syntax:"
    , ""
    , "  on key_press key_name: {"
    , "      statements"
    , "  }"
    , ""
    , "Example:"
    , ""
    , "  on key_press space: {"
    , "      jump()"
    , "  }"
    , ""
    , "The event handler contains ordinary LumenScript statements."
    , ""
    , "Multiple handlers can be declared in the same program."
    , ""
    , "The simulator triggers registered handlers through keyboard input."
    , ""
    , "Currently documented event support is limited to key presses."
    ])


functions :: ManualSection
functions = ManualSection
  "functions"
  "Function and Method Calls"
  (unlines
    [ "Functions allow a program to perform operations."
    , ""
    , "Function Call:"
    , ""
    , "  print(\"Hello!\")"
    , "  jump()"
    , ""
    , "Method Call:"
    , ""
    , "  player.jump()"
    , ""
    , "Calls can also receive arguments:"
    , ""
    , "  print(\"Player defeated!\")"
    , ""
    , "The language supports ordinary function calls and"
    , "object-method calls."
    , ""
    , "Available functions and methods depend on the engine API."
    , "See the Engine API section and the Engine API Explorer for"
    , "the available members."
    ])


engineApi :: ManualSection
engineApi = ManualSection
  "engine-api"
  "Engine API Reference"
  (unlines $
    [ "This reference is generated from the engine API catalog."
    , "The catalog is built from HostBindings, so new engine members"
    , "appear here automatically alongside the API Explorer."
    , ""
    ] ++ concatMap renderApiObject apiCatalog)

renderApiObject :: ApiObject -> [String]
renderApiObject object =
  [ titleFor object
  , ""
  , "  " ++ headingFor object
  , "  " ++ separator
  ] ++
  map (("  " ++) . renderApiMember) (aoMembers object) ++
  [ ""
  , "  Example:"
  ] ++
  map ("    " ++) (categoryExample (aoName object) (aoMembers object)) ++
  [ "" ]
  where
    separator = replicate 78 '-'

titleFor :: ApiObject -> String
titleFor object
  | null (aoName object) = "Global Functions"
  | otherwise            = objectTitle object

headingFor :: ApiObject -> String
headingFor object
  | null (aoName object) = "Function             Type    Description"
  | otherwise            = "Member               Type    Description"

renderApiMember :: ApiMember -> String
renderApiMember member =
  padRight 21 (apiSyntax member) ++ " " ++
  padRight 7 (maybe "void" tyName (amType member)) ++ " " ++
  amDoc member

apiSyntax :: ApiMember -> String
apiSyntax member =
  qualifiedName member ++ case amKind member of
    KindProperty -> ""
    _ -> "(" ++ intercalate ", " (map fst (amParams member)) ++ ")"

padRight :: Int -> String -> String
padRight width value = value ++ replicate (max 1 (width - length value)) ' '

categoryExample :: String -> [ApiMember] -> [String]
categoryExample "player" _ =
  [ "let progress = player.distance"
  , "let currentLevel = player.level"
  , "player.jump()"
  ]
categoryExample "enemy" _ =
  [ "on key_press e: {"
  , "    enemy.patrol()"
  , "    enemy.attack()"
  , "}"
  ]
categoryExample "npc" _ =
  [ "npc.say(\"Hello, traveler!\")" ]
categoryExample "" _ =
  [ "print(\"Player jumped!\")"
  , "jump()"
  , "dash()"
  ]
categoryExample _ (member:_) =
  [ insertText member ]
categoryExample _ [] = [ "# No members are currently available." ]


comments :: ManualSection
comments = ManualSection
  "comments"
  "Comments and Statement Separators"
  (unlines
    [ "Comments"
    , ""
    , "Use # to write a single-line comment."
    , ""
    , "  # This is a comment"
    , "  let score = 10  # Initial score"
    , ""
    , "Comments are ignored by the lexer."
    , ""
    , "Statement Separators"
    , ""
    , "Statements can be separated using newlines or semicolons."
    , ""
    , "Example:"
    , ""
    , "  let health = 100"
    , "  let lives = 3"
    , ""
    , "Or:"
    , ""
    , "  let health = 100; let lives = 3"
    ])


examples :: ManualSection
examples = ManualSection
  "examples"
  "Complete Examples"
  (unlines
    [ "Example 1: Platformer Controls"
    , ""
    , "  let jumps = 2"
    , "  let canDash = true"
    , ""
    , "  on key_press space: {"
    , "      if jumps > 0: {"
    , "          jump()"
    , "          jumps = jumps - 1"
    , "      } else: {"
    , "          print(\"No jumps remaining.\")"
    , "      }"
    , "  }"
    , ""
    , "  on key_press shift: {"
    , "      if canDash: {"
    , "          dash()"
    , "          canDash = false"
    , "      } else: {"
    , "          print(\"Dash unavailable.\")"
    , "      }"
    , "  }"
    , ""
    , "This example demonstrates variables, conditions, assignments,"
    , "function calls, and keyboard events."
    , ""
    , "Example 2: Enemy AI"
    , ""
    , "  let state = 0"
    , "  let distance = 12"
    , ""
    , "  on key_press p: {"
    , "      if state == 0: {"
    , "          print(\"Enemy is patrolling.\")"
    , "          if distance < 10: {"
    , "              state = 1"
    , "          }"
    , "      } else: {"
    , "          print(\"Enemy is chasing.\")"
    , "          if distance > 15: {"
    , "              state = 0"
    , "          }"
    , "      }"
    , "  }"
    , ""
    , "This example demonstrates state-based conditional behaviour."
    , ""
    , "Example 3: Dialogue Tree"
    , ""
    , "  let dialogue = 0"
    , ""
    , "  on key_press e: {"
    , "      if dialogue == 0: {"
    , "          print(\"Hello, traveler!\")"
    , "          print(\"Choose your path.\")"
    , "          dialogue = 1"
    , "      } else: {"
    , "          print(\"Good luck on your journey!\")"
    , "      }"
    , "  }"
    , ""
    , "This example demonstrates how a variable can track dialogue state."
    ])


limitations :: ManualSection
limitations = ManualSection
  "limitations"
  "Current Language Limitations"
  (unlines
    [ "The following features are not currently supported:"
    , ""
    , "  - Logical AND (&&) and OR (||) operators."
    , "  - elif conditional branches."
    , "  - Array expressions and array operations."
    , "  - Assignment to object members."
    , "  - Collision, update, and interact event handlers."
    , ""
    , "Some of these keywords or AST representations may exist internally,"
    , "but that does not mean they are available language features."
    , ""
    , "Consult future versions of the manual as the language evolves."
    ])


troubleshooting :: ManualSection
troubleshooting = ManualSection
  "troubleshooting"
  "Errors and Troubleshooting"
  (unlines
    [ "Compilation Errors"
    , ""
    , "Unexpected token:"
    , "  Check the syntax of the statement and its surrounding tokens."
    , ""
    , "Malformed declaration:"
    , "  Ensure that declarations follow the form:"
    , "  let identifier = expression"
    , ""
    , "Missing closing brace:"
    , "  Check that every opening { has a matching }."
    , ""
    , "Invalid assignment:"
    , "  Ensure that the assignment target is a valid variable."
    , ""
    , "Immutable variable reassignment:"
    , "  Variables declared with const cannot be modified."
    , ""
    , "Runtime Errors"
    , ""
    , "Undefined variable:"
    , "  Ensure that the variable has been declared before use."
    , ""
    , "Type error:"
    , "  Check that the operators are used with compatible values."
    , ""
    , "If an error persists, inspect the compiler's error panel"
    , "for the reported stage and source location."
    ])