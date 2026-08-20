# LumenScript Studio

A custom scripting language and IDE for game development, built with Haskell. This project provides the tools to write, parse, and eventually execute game logic scripts.

## Code Owners

- Arpita Swain
- Aurabhri Sharma
- Manshi Singh

## Getting Started

This project is built using the [Haskell Stack tool](https://docs.haskellstack.org/en/stable/README/). All commands should be run from the `lumenscript-studio` directory.

### Prerequisites

Ensure you have [Stack](https://docs.haskellstack.org/en/stable/install_and_upgrade/) installed on your system.

### Building the Project

To build the project and all its dependencies, run the following command:

```bash
stack build
```

### Running Tests

The project includes a test suite to verify the functionality of the lexer, parser, and other components. To run the tests, use:

```bash
stack test
```

### Running Demos

To see the parser in action with example scripts, you can run the following commands:

```bash
stack run lumenscript-studio-exe -- demos/dialogue_tree.lum
stack run lumenscript-studio-exe -- demos/enemy_ai.lum
stack run lumenscript-studio-exe -- demos/platformer.lum
```

---

## Project Structure

- `src/`: Contains the core source code for the language
  - `Lexer/`: Handles tokenization.
  - `Parser/`: Builds the Abstract Syntax Tree (AST).
  - `Semantic/`: Performs type checking and validation.
  - `Bytecode/`: Bytecode generation module (in progress)
  - `GUI/`: IDE components
  - `Simulation/`: Game simulation logic
  - `VM/`: Virtual machine implementation (not started)
- `test/`: Contains the test suites for all components
- `demos/`: Example `.lum` scripts demonstrating language features
- `lumenscript-studio.cabal`: The main package definition file

---

## LumenScript by Example

Here is a simple script showing an event handler that moves a player entity.

_Move the player to the right on every frame update_

```lumenscript
on update: player.x = player.x + 1
```

_On collision with a coin, increase the score_

```lumenscript
on collision "player" "coin": score = score + 10
```

## Project Roadmap

- [x] **Lexer**: Tokenizes the source code. (Complete and documented)
  - Complete (lexer-v1.0-beta)
- [x] **Parser**: Builds an Abstract Syntax Tree (AST) from tokens.
  - Complete (parser-v1.0-beta)
- [x] **Semantic Analysis**: Performs type checking and validation.
  - Complete with 124 passing tests (semantic-v1.0-beta)
- [x] **Bytecode Compiler**: Compiles the AST to an intermediate representation.
  - **Status**: Complete (compiler-v1.0-beta).
  - **Details**: The end-to-end compiler pipeline is complete, integrating the lexer, parser, semantic analysis, and bytecode generation. All tests pass and demo scripts compile successfully.
- [ ] **Virtual Machine (VM)**: Executes the compiled bytecode.
  - Not started yet

---

## Lexer Token Set (v1.0)

The following table documents the complete set of tokens produced by the lexer. This serves as the official contract for the parser development phase.

| Token Constructor | Description                              | Example(s)                   |
| ----------------- | ---------------------------------------- | ---------------------------- |
| `TInt Int`        | An integer literal.                      | `123`, `42`, `0`             |
| `TString String`  | A string literal.                        | `"Hello"`, `"Game Over"`     |
| `TBool Bool`      | A boolean literal.                       | `true`, `false`              |
| `TIdent String`   | An identifier (variable, function name). | `player`, `health`, `jump`   |
| `TDot`            | The dot operator for member access.      | `.`                          |
| `TComma`          | The comma separator.                     | `,`                          |
| `TLParen`         | A left parenthesis.                      | `(`                          |
| `TRParen`         | A right parenthesis.                     | `)`                          |
| `TColon`          | A colon, used for blocks.                | `:`                          |
| `TAssign`         | The assignment operator.                 | `=`                          |
| `TPlus`           | The addition operator.                   | `+`                          |
| `TMinus`          | The subtraction operator.                | `-`                          |
| `TMultiply`       | The multiplication operator.             | `*`                          |
| `TDivide`         | The division operator.                   | `/`                          |
| `TMod`            | The modulo operator.                     | `%`                          |
| `TLt`             | The less than operator.                  | `<`                          |
| `TGt`             | The greater than operator.               | `>`                          |
| `TEq`             | The equality operator.                   | `==`                         |
| `TNotEqual`       | The inequality operator.                 | `!=`                         |
| `TLe`             | The less than or equal to operator.      | `<=`                         |
| `TGe`             | The greater than or equal to operator.   | `>=`                         |
| `TKwLet`          | The `let` keyword.                       | `let`                        |
| `TKwIf`           | The `if` keyword.                        | `if`                         |
| `TKwElse`         | The `else` keyword.                      | `else`                       |
| `TKwElif`         | The `elif` keyword.                      | `elif`                       |
| `TKwOn`           | The `on` keyword for event handlers.     | `on`                         |
| `TKwKeyPress`     | The `key_press` keyword.                 | `key_press`                  |
| `TKwCollision`    | The `collision` keyword.                 | `collision`                  |
| `TKwUpdate`       | The `update` keyword.                    | `update`                     |
| `TKwInteract`     | The `interact` keyword.                  | `interact`                   |
| `TIndent`         | Represents an increase in indentation.   | (Generated by the lexer)     |
| `TDedent`         | Represents a decrease in indentation.    | (Generated by the lexer)     |
| `TNewline`        | A newline character.                     | `\n`                         |
| `TEOF`            | End of the input file.                   | (Generated by the lexer)     |
| `TError String`   | Represents a lexing error.               | (Generated on invalid input) |
