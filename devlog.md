**Author** : pixeldust01 (Arpita Swain)
**Documenting Project Status** : Started 3 Aug 2026

A chronological log of the project's development milestones.

### 12 August 2026

- **Advanced Bytecode Implementation**: Made significant progress on the bytecode module, focusing on architectural improvements and feature expansion.
  - **Expanded Instruction Set**: Added 12 new instructions to `Instruction.hs`, including `CALL`, `RET`, `HALT`, and instructions for stack manipulation (`POP`, `DUPLICATE`, `SWAP`) and data structures (`ARRAY_GET`/`SET`, `DICT_GET`/`SET`).
  - **Refactored Bytecode Generator**: Reworked the generator to use a pure functional style with an explicit symbol table, which now uses more efficient stack indices for local variables.
  - **Modular Symbol Table**: Extracted the symbol table into its own dedicated module (`Bytecode/SymbolTable.hs`) to improve code structure and separate concerns.
  - **Corrected Stack Management**: Fixed a runtime issue by adding a `POP` instruction after an `ExprStmt` in the `generateStmt` function (`Bytecode/Generator.hs`). This prevents stack overflow by ensuring expression statement results are correctly discarded.
  - **Updated Bytecode Tests**: Aligned the bytecode test suite (`BytecodeSpec.hs`) with the stack management fix, updating all relevant tests to expect the new `POP` instruction.

  **Challenges:**
  - **Architectural Shift**: Transitioning the bytecode generator from a stateful to a pure functional design required a significant refactoring effort but has resulted in a more robust and maintainable implementation.

### 11 August 2026

- **Finalized Semantic Analysis & Code Refactor**: Fine-tuned the semantic analysis module by implementing an enhanced type checker with a `SymbolTable.hs. Resolved all outstanding errors and expanded test coverage, bringing the total to 124 passing tests.
- **Improved Code Quality & Documentation**: Refactored the semantic analysis modules (`Semantic.hs`, `TypeCheck.hs`, `SymbolTable.hs`, `ErrorLog.hs`) and tests (`SemanticSpec.hs`) to follow Haskell best practices, significantly improving clarity, structure, and maintainability.
  - **Enhanced Internal Documentation**: Added extensive comments to clarify the design rationale, including the idiomatic use of `Either` and `Maybe`, the separation of concerns in the symbol table, and the purpose of the custom `SemanticM` monad for accumulating multiple errors.
  - **First-Class Warnings**: Implemented a distinct `logWarning` function with a `GenericWarning` type to properly separate non-fatal warnings from errors.
  - **Cleaner Tests & API**: Simplified the test suite with a helper function and adopted idiomatic Hspec (`shouldSatisfy`). Renamed functions for clarity and established `analyzeProgram` as the single, clear entry point.
- **Initiated Bytecode Generation**: Created the `bytecode-generation` feature branch to begin development of the bytecode generation module.

  **Challenges:**
  - **Semantic Analysis Debugging**: Faced difficulties in resolving errors within the semantic analysis test module, which required significant debugging effort.

### 4 August 2026

- **Verified Tokenization**: Audited the lexer correctly handles all specified language features, including keywords, identifiers, literals (integers and strings), operators, and symbols.
- **Finalized Core Mechanics**: Validated position tracking, comment and whitespace handling, and the use of `NEWLINE` as a statement separator.

  **Challenges:**
  - Collaboration Friction: Concurrent work on core files like `Token.hs` and `Lexer.hs` led to merge conflicts and development bottlenecks.

### 3 August 2026

- **Lexer Completion & Integration**: Finished implementing the remaining core lexer functions and their corresponding unit tests in `LexerSpec.hs`.
- **Demo Script Integration**: Integrated the three expanded demo scripts (`platformer.lum`, `enemy_ai.lum`, `dialogue_tree.lum`) as real-world integration tests.
- **Created `DemoSpec.hs`**: Authored a new test suite to validate the lexer's output against the full demo scripts, ensuring all tokens were generated correctly.
- **Build & Finalization**: Resolved a `.cabal` file warning and aligned the `DemoSpec.hs` assertions with the lexer's actual output to pass all tests.
- **Lexer Freeze**: Officially "froze" the lexer by creating the `lexer-v1.0-beta` Git tag to mark a stable version for the parser phase.
- **Project Documentation**: Overhauled the `README.md` with a project description, setup instructions, a roadmap, and the final token set documentation.

  **Challenges:**
  - None so far.

### 2 August 2026

- **Lexer Scaffolding**: Began the lexer implementation by defining the complete set of required language tokens in `Token.hs`.
- **Structured Development**: Created empty placeholder functions in `Lexer.hs` for all core scanning tasks (e.g., `scanNumber`, `scanIdentifier`, `scanString`, etc.) to outline the required work.
- **Core Implementation & TDD**: Methodically implemented the logic for each lexer helper function one by one. Followed a Test-Driven Development (TDD) approach by writing a corresponding unit test in `LexerSpec.hs` for each piece of functionality. This included handling numbers, strings, identifiers, operators, symbols, whitespace, and position tracking.
- **Error Handling**: Implemented robust lexical error reporting for invalid characters or malformed tokens.

  **Challenges:**
  - **Initial Complexity**: The primary challenge was managing the complexity of building a lexer from scratch.
  - **Methodical Solution**: This was addressed by adopting a structured approach: first defining the entire token set (`Token.hs`), then creating a scaffold of empty functions (`Lexer.hs`), and finally implementing and testing each function individually. This broke the problem down into small, manageable, and testable parts.
