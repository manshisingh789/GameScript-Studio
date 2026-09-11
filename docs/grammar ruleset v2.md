## LumenScript v1 Final Design Principles

I agree with the latest design decisions. They make the language more Python-inspired while keeping the compiler implementation manageable.

One adjustment: not using semicolons. Since this is a Python-like language, statements can naturally end at the end of a line. The lexer can emit a NEWLINE token (or simply treat line breaks as statement separators), while still using braces {} to delimit blocks. This gives the readability of Python without the complexity of indentation-sensitive parsing.

## Language Philosophy

- Python-inspired syntax

- Beginner-friendly

- Minimal punctuation

- Explicit variable declarations using let (mutable) and const (immutable)

- No semicolons (;)

- Braces {} for blocks (simplifies parsing and debugging)

- Type inference (no explicit types)

- Event-driven scripting

- Object-style game API (player.jump())

## Supported Features

## Variables

LumenScript supports both mutable and immutable variable declarations, giving you control over how your data can be modified.

### Mutable Variables (`let`)

Use `let` for variables whose values you expect to change during the program's execution.

**Example:**

```
let health = 100
health = health - 10 // Health can be updated
```

### Immutable Variables (`const`)

Use `const` for variables that should not be reassigned after they are initialized. This is useful for defining constants like gravity, configuration settings, or any value that should remain fixed.

**Example:**

```
const gravity = 9.8
const game_name = "Lumenia"
```

Attempting to reassign a `const` variable will be caught by the compiler and result in an error, which helps prevent bugs.

## Assignment

```
health = health - 10
score = score + 5
```

## Arithmetic

```
Supported operators: + - * / %
```

## Comparisons

== != < > <= >=

## Booleans

true / false

## Conditionals

if health <= 0: {

dialogue.show("Game Over")

}

## Events

on key_press SPACE: {

player.jump()

}

## Function Calls

player.jump()

enemy.attack()

dialogue.show("Hello")

## Comments

\# This is a comment

## EBNF Grammar

```
program = { statement } ;
statement = declaration
| assignment
| eventStatement
| ifStatement
| functionCall ;
declaration = ("let" | "const") identifier "=" expression ;
assignment = identifier "=" expression ;
eventStatement = "on" "key_press" identifier ":" block ;
ifStatement = "if" expression ":" block
[ "else" ":" block ] ;
block = "{"
{ statement }
"}" ;
functionCall = member "(" [ argumentList ] ")" ;
argumentList = expression
{ "," expression } ;
expression = comparison ;
comparison = addition
[ comparisonOperator addition ] ;
addition = multiplication
{ additionOperator multiplication } ;
multiplication = unary
{ multiplicationOperator unary } ;
unary = [ "-" | "!" ] primary ;
primary = integer
| string
| boolean
| identifier
| member
| "(" expression ")" ;
member = identifier "." identifier ;
boolean = "true"
| "false" ;
additionOperator = "+" | "-" ;
multiplicationOperator = "*" | "/" | "%" ;
```

comparisonOperator

= "==" | "!=" | "<" | ">" | "<=" | ">=" ;

integer

= digit { digit } ;

string

= '"' { character } '"' ;

identifier

=

letter

{ letter | digit | "\_" } ;

## Example Program

```
\# Player setup
let health = 100
let score = 0
on key_press SPACE:
{
player.jump()
}
on key_press LEFT:
{
player.moveLeft()
}
if health <= 0:
{
dialogue.show("Game Over")
}
```

## AST Produced

The AST is updated to distinguish between mutable (`let`) and immutable (`const`) declarations. This allows the semantic analyzer to enforce immutability.

```
Program
|
+-- ConstDeclaration      // Represents an immutable binding
| starting_health = 100
|
+-- LetDeclaration        // Represents a mutable binding
| health = starting_health
|
+-- LetDeclaration
| score = 0
|
+-- Event
| key_press SPACE
| +-- FunctionCall
| player.jump()
|
+-- Event
| key_press LEFT
| +-- FunctionCall
| player.moveLeft()
|
+-- If
health <= 0
+-- FunctionCall
dialogue.show("Game Over")
```

## Compiler Features Demonstrated

| Stage               | Demonstrated By                                                                           |
| ------------------- | ----------------------------------------------------------------------------------------- |
| Lexical Analysis    | Keywords, identifiers, literals, operators                                                |
| Parsing             | Statements, expressions, events, blocks                                                   |
| AST Generation      | Hierarchical syntax tree                                                                  |
| Semantic Analysis   | Variable declarations, undefined variables, type checking, immutability check for `const` |
| Bytecode Generation | Arithmetic, assignments, comparisons, jumps, calls                                        |
| Virtual Machine     | Stack execution, branching, event dispatch                                                |
| Game Engine         | Event-driven game scripting                                                               |

## One Recommendation

Keep parentheses for function calls (player.jump()) because they make the grammar extensible. Even though jump currently takes no arguments, using parentheses now allows support for future calls like:

dialogue.show("Welcome")

enemy.moveTo(100, 200)

player.move(5)

This keeps the language consistent and avoids a breaking syntax change in future versions. Overall, this version strikes a good balance between a Python-inspired user experience and a compiler that is realistic to implement in Haskell within the project timeline.
