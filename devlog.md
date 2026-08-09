**Author** : pixeldust01 (Arpita Swain)
**Documenting Project Status** : Started 3 Aug 2026

A chronological log of the project's development milestones.

### 6 August 2026

- [Arpita] **Prototyped Bytecode Generation**: Implemented a prototype of the bytecode generator (`Generator.hs`) and redesigned the instruction set (`Instruction.hs`). This work was done in isolation, using dummy values, as the semantic analysis module was not yet complete.
- [Arpita] **Added Foundational Tests**: Created initial tests for the bytecode generator to validate core functionality like variable handling, arithmetic, and control flow.
- [Arpita] **Minor Test Suite Cleanup**: Reordered the test execution pipeline in `Spec.hs`.

  **Challenges:**
  - **Dependency Delays**: The semantic analysis module, a key dependency, was incomplete due to a team member's illness and subsequent reassignment. This required the bytecode generator to be developed as a prototype, with the understanding that it will need to be integrated properly later.
  - **State Management**: Managing state across different compilation units in the generator was complex but was addressed by using a monadic approach.

### 4 August 2026

- [Arpita] **Verified Tokenization**: Audited the lexer correctly handles all specified language features, including keywords, identifiers, literals (integers and strings), operators, and symbols.
- [Arpita] **Finalized Core Mechanics**: Validated position tracking, comment and whitespace handling, and the use of `NEWLINE` as a statement separator.

  **Challenges:**
  - Collaboration Friction: Concurrent work on core files like `Token.hs` and `Lexer.hs` led to merge conflicts and development bottlenecks.

### 3 August 2026

- [Arpita] **Lexer Completion & Integration**: Finished implementing the remaining core lexer functions and their corresponding unit tests in `LexerSpec.hs`.
- [Arpita] **Demo Script Integration**: Integrated the three expanded demo scripts (`platformer.lum`, `enemy_ai.lum`, `dialogue_tree.lum`) as real-world integration tests.
- [Arpita] **Created `DemoSpec.hs`**: Authored a new test suite to validate the lexer's output against the full demo scripts, ensuring all tokens were generated correctly.
- [Arpita] **Build & Finalization**: Resolved a `.cabal` file warning and aligned the `DemoSpec.hs` assertions with the lexer's actual output to pass all tests.
- [Arpita] **Lexer Freeze**: Officially "froze" the lexer by creating the `lexer-v1.0-final` Git tag to mark a stable version for the parser phase.
- [Arpita] **Project Documentation**: Overhauled the `README.md` with a project description, setup instructions, a roadmap, and the final token set documentation.

  **Challenges:**
  - None so far.

### 2 August 2026

- [Arpita] **Lexer Scaffolding**: Began the lexer implementation by defining the complete set of required language tokens in `Token.hs`.
- [Arpita] **Structured Development**: Created empty placeholder functions in `Lexer.hs` for all core scanning tasks (e.g., `scanNumber`, `scanIdentifier`, `scanString`, etc.) to outline the required work.
- [Arpita] **Core Implementation & TDD**: Methodically implemented the logic for each lexer helper function one by one. Followed a Test-Driven Development (TDD) approach by writing a corresponding unit test in `LexerSpec.hs` for each piece of functionality. This included handling numbers, strings, identifiers, operators, symbols, whitespace, and position tracking.
- [Arpita] **Error Handling**: Implemented robust lexical error reporting for invalid characters or malformed tokens.

  **Challenges:**
  - **Initial Complexity**: The primary challenge was managing the complexity of building a lexer from scratch.
  - **Methodical Solution**: This was addressed by adopting a structured approach: first defining the entire token set (`Token.hs`), then creating a scaffold of empty functions (`Lexer.hs`), and finally implementing and testing each function individually. This broke the problem down into small, manageable, and testable parts.
