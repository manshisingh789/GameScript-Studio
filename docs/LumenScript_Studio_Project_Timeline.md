# LumenScript Studio — Project Timeline
**Team of 3 | Duration: 28 July 2026 – 30 November 2026 (~18 weeks)**

## How this timeline is structured

Since you're working **module-by-module as a team** (not three parallel tracks), the timeline is sequential: each pipeline stage is a phase, with the assigned "lead" driving implementation while the other two actively build alongside, review, and write tests. This matches how the pipeline is genuinely coupled — Person 2 can't build the bytecode generator without Person 1's AST types being stable, and Person 3 can't wire the VM without Person 2's bytecode format being final.

**Roles recap:**
| Person | Leads | Modules |
|---|---|---|
| Person 1 | Lexer + Parser + AST | P1, P2 |
| Person 2 | Semantic Analysis + Bytecode Gen | P3, P4 |
| Person 3 | VM + Simulation + GUI + Integration | P5, Sim, UI |

**Working agreement suggestion:** since all three sit on every module, agree on a lightweight cadence now — e.g. a short daily/every-other-day check-in, one person "drives" the keyboard per session on a rotating basis, and nobody starts the next module until the current one has passing unit tests. This avoids the classic failure mode of module-by-module teams: three people quietly building slightly different mental models of the same data structure.

---

## Month 1 — August 2026: Lexer, Parser, AST
*Lead: Person 1 | Supporting: Person 2 & 3*

| Week | Dates | Work |
|---|---|---|
| 0 | Jul 28 – Aug 3 | **Setup week.** Lock in the LumenScript grammar per the approved Final Design doc — `let` variable declarations, assignment, arithmetic (`+ - * / %`), comparisons, boolean literals, brace `{}` blocks, `on`/`if`-`else` syntax, `#` comments, newline-based statement separation. Set up GHC/Stack/Cabal project skeleton, Git repo + branch strategy, CI (GitHub Actions) running `stack test` on push. Agree on the shared `Token` and `AST` data type signatures now (including the `NEWLINE` token and `Decl`/`Assign` statement forms), since Person 2 and 3's later work depends on these not changing. |
| 1 | Aug 4 – 10 | Build the **Lexer**: token types, identifier/number/operator/keyword scanning, whitespace & comment skipping, line/column tracking. Hspec unit tests for valid + malformed input. |
| 2 | Aug 11 – 17 | Finish lexer edge cases (invalid character errors with line/col). Start **Parser** scaffolding with Parsec/Megaparsec — literals, variables, binary operations. |
| 3 | Aug 18 – 24 | Parser: if-else and function-call parsing, plus `let` declarations and assignment. Finalize shared **AST ADTs** (`Stmt` covering `Decl`/`Assign`/`EventStmt`/`IfStmt`/`CallStmt`, `Expr` covering int/string/bool literals, `Var`, `Member`, `BinOp`, `UnaryOp`). Syntax error reporting (expected-token messages). |
| 4 | Aug 25 – 31 | Integration pass: run all 3 demo scripts (Platformer, Enemy AI, Dialogue Tree source) through lexer→parser, confirm correct token streams and ASTs. Freeze AST/Token interfaces. |

**🎯 Milestone 1 (Aug 31):** Source scripts reliably tokenize and parse into correct ASTs, with proper error reporting. AST data type is frozen for the rest of the project.

---

## Month 2 — September 2026: Semantic Analysis, Bytecode Generation
*Lead: Person 2 | Supporting: Person 1 & 3*

| Week | Dates | Work |
|---|---|---|
| 5 | Sep 1 – 7 | **Symbol Table** design + **Scope Resolution** pass over the AST. |
| 6 | Sep 8 – 14 | **Type Checking** + **Declaration Verification**. **Error aggregation** + Error Log structure, so semantic errors surface cleanly instead of crashing the pipeline. |
| 7 | Sep 15 – 21 | Define **Bytecode Instruction** data type (opcode + operand). Implement `generateBytecode()` for Literal, Variable, BinaryOp per the pseudocode in the SDS. |
| 8 | Sep 22 – 30 | Extend bytecode generation to IfElse (label/jump logic) and FunctionCall. Integration pass: run demo scripts source→AST→validated AST→bytecode end-to-end; verify against hand-traced expected bytecode. |

**🎯 Milestone 2 (Sep 30):** Full front-half of the pipeline works — a LumenScript source file reliably compiles to a correct bytecode listing, with semantic errors caught and reported before codegen.

---

## Month 3 — October 2026: Virtual Machine, Simulation Layer, GUI
*Lead: Person 3 | Supporting: Person 1 & 2*

| Week | Dates | Work |
|---|---|---|
| 9 | Oct 1 – 7 | **VM core**: instruction pointer, operand stack (push/pop/eval), fetch-decode-execute loop for `PUSH`, `LOAD`, `OP`. |
| 10 | Oct 8 – 14 | VM branching: `JUMP`, `JUMP_IF_FALSE`, `LABEL` resolution, `CALL`. Runtime error handling (stack underflow, invalid instruction). |
| 11 | Oct 15 – 21 | **Event Trigger** dispatch mechanism + **Simulation Layer** (grid world / sprite state) reacting to VM events. Step-by-step and continuous run modes. |
| 12 | Oct 22 – 31 | **GUI scaffolding** (Threepenny-GUI/Reflex or Electron wrapper): script editor panel, and skeleton Token/AST/Bytecode/Stack-trace viewer panels (can render static/dummy data at first). |

**🎯 Milestone 3 (Oct 31):** VM correctly executes bytecode standalone (verified with hand-written bytecode test files) and drives simulation state changes. GUI shell is running with all panels present, even if not yet wired to live data.

---

## Month 4 — November 2026: Full Integration, Testing, Polish
*All three together — this is genuinely shared work*

| Week | Dates | Work |
|---|---|---|
| 13 | Nov 1 – 7 | **Wire the full pipeline**: Editor → Lexer → Parser → Semantic Analyzer → Bytecode Generator → VM → GUI panels → Simulation, using real (not dummy) data at every stage. This is the highest-coordination week — expect this to take longer than planned, hence week 15 buffer below. |
| 14 | Nov 8 – 14 | Get all **3 demo scenarios** (Platformer, Enemy AI, Dialogue Tree) working end-to-end through the GUI. Polish visualisations: live token highlighting, navigable AST tree, readable bytecode listing, live IP/stack view. |
| 15 | Nov 15 – 21 | **System, integration, regression, and stress testing** (per SDS Section 5): malformed scripts at every stage, deeply nested conditionals, stack growth checks. Fix bugs surfaced by testing. *(This week doubles as schedule buffer — if Week 13 integration overruns, it absorbs here.)* |
| 16 | Nov 22 – 30 | Final polish, acceptance-test dry run with the 3 demos, finalize documentation (update SRS/SDS to match as-built system), prepare demo/viva walkthrough. |

**🎯 Milestone 4 (Nov 30):** LumenScript Studio runs all three demo scenarios end-to-end through the GUI, handles errors gracefully at every stage, and the team can walk an evaluator through the live pipeline.

---

## Summary Gantt (by week)

```
Week:        1  2  3  4  5  6  7  8  9  10 11 12 13 14 15 16
Lexer/Parser [██████████]
Semantic/BC              [██████████]
VM/Sim/GUI                           [██████████]
Integration                                      [████████]
Testing/Buf                                            [████]
Docs/Demo                                                  [██]
```

## Key risks to watch
- **Interface drift**: if Person 1's AST or Person 2's bytecode format changes after being "frozen," it cascades into rework for whoever's downstream. Treat the Milestone 1 and 2 freezes as real commitments — extend rather than silently redesign.
- **Integration week (13) is the highest-risk week** in the whole project, since it's the first time all modules run against each other's *real* output instead of hand-crafted test data. Week 15 is deliberately kept lighter to absorb overflow from it.
- **GUI framework choice** (Threepenny-GUI vs Reflex vs Electron) should be locked in during Week 0 setup, not revisited in October — switching frameworks mid-Month-3 would consume the entire buffer.
