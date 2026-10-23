# LumenScript Studio v2 — Project Timeline
**Team of 3 | Second development phase, building on the completed v1 platform**
**Duration: 1 December 2026 – 25 January 2027 (~8 weeks)**

---

## Why this is a separate document

`LumenScript_Studio_Project_Timeline.md` and `LumenScript_Studio_Team_Roadmap.md` cover **v1**: the 18-week build (28 Jul – 30 Nov 2026) that took LumenScript Studio from nothing to three working demo scenarios (Platformer Logic, Enemy AI, Dialogue Tree) running end-to-end through a GUI, per Milestone 4. That work is done and is **not reopened here**.

This document covers **v2**: turning the tiered feature list in `LumenScript_Studio_Sprint_Plan.md` into a working, tested, application-ready release, exactly as the SRS's own `6.4 Future Enhancements` anticipated. It starts the day after v1's Milestone 4 and uses the same weekly-table format as the v1 timeline so the team can read it the same way.

Nothing below touches the frozen v1 contracts (`Token.hs`, `AST.hs`, `HostBindings.hs`, `Bytecode/Instruction.hs`) or their existing behaviour. v2 is **additive**: new modules that consume the existing pipeline's output, plus a regression pass to prove the original three demos still behave identically once v2 is wired in.

---

## Starting point: what v2 builds on

From the v1 roadmap, frozen and unchanged:
- **Pipeline**: `Lexer.hs → Parser.hs → Semantic/* → Bytecode/* → VM.hs → GUI/* → Simulation/*`, communicating via the frozen `Token`/`AST`/`Instruction`/`HostBindings` types.
- **Three working demos**: Platformer Logic, Enemy AI, Dialogue Tree, each already compiling and running correctly end-to-end.
- **Architecture**: Presentation Layer / Compiler & Runtime Layer / Simulation Layer, per the SDS.

v2 does not redesign any of this — every Tier S/A/B feature is a new module that reads from or wraps an existing one (e.g. `StepControl.hs` wraps the existing `VM.hs` loop rather than replacing it).

---

## Roles recap: v1 assignment → v2 ownership

The Sprint Plan defines its ownership by architecture ("Compiler/Language Owner," "VM/Simulation Owner," "GUI/Educational UX Owner") rather than by name. Mapped onto the actual team using the v1 roadmap for continuity:

| Team Member | v1 Role (Roadmap) | v2 Ownership (Sprint Plan) | Why this mapping |
|---|---|---|---|
| **Arpita Swain** | Person 2 — Semantic Analysis + Bytecode Generation | **Compiler / Language Owner** | Directly continues her v1 domain — the symbol table and error-aggregation logic she already built are exactly what the structured-error and "why"-explanation work extends. |
| **Aurabhri Sharma** | Person 3 — VM + Simulation + GUI + Integration | **VM / Simulation Owner** | Narrows her v1 scope to VM + Simulation only, continuing to own `VM.hs`, `Stack.hs`, `EventTrigger.hs`, and `Simulation/*` directly. |
| **Manshi Singh** | Person 1 — Lexer + Parser + AST | **GUI / Educational UX Owner** | Her v1 files (`Token.hs`, `Lexer.hs`, `AST.hs`, `Parser.hs`) were frozen at the end of v1 Week 4 and need no further ownership, so she picks up the GUI half of what Aurabhri owned alone in v1 — parallelising the two-person job Aurabhri did solo before. |

*(If your team actually split it differently, only this table needs to change — the weekly plan below is written in terms of the three roles, not the names.)*

**Working agreement (unchanged from v1):** short check-ins every day or two, one shared branch strategy, and nobody starts integrating a tier until its own unit tests pass — the same discipline that avoided interface drift in v1.

---

## Shared contracts to freeze in Week 1

Exactly as `Token.hs`/`AST.hs`/`HostBindings.hs` were frozen in v1 Week 0, these five new types are the foundation everything else in v2 depends on. They must not shift after Week 1:

| File (new unless noted) | Owner | Purpose |
|---|---|---|
| `src/Diagnostics/StructuredError.hs` | Arpita | `data StructuredError = StructuredError { stage :: Stage, errLine :: Int, errCol :: Int, message :: String, why :: String }` and `data Stage = Lexing \| Parsing \| Semantic \| Bytecode`, JSON-exportable so the GUI and VM debugger consume one shared shape instead of each inventing their own. |
| `Semantic/SymbolTable.hs` (extended) | Arpita | A JSON-exportable dump of the native/global symbol table, so the API Explorer and Language Manual read the same table the compiler already maintains rather than a hand-kept second copy. |
| `src/VM/StepControl.hs` | Aurabhri | `data ExecMode = Stepping \| Running \| Paused \| ResetMode` wrapping the existing fetch-decode-execute loop in `VM.hs`, plus a `VMSnapshot { ip, stack, vars }` type emitted after every instruction for the GUI to poll or subscribe to. |
| `src/Simulation/StateTransition.hs` | Aurabhri | A `StateEvent` type capturing Dialogue Tree / Enemy AI state changes as they happen, decoupled from how they'll eventually be rendered. |
| `src/GUI/PipelineHighlight.hs` | Manshi | The shared span-mapping type that keeps Source/Token/AST/Bytecode selection in sync — the single type every highlighting feature reads from. |

---

## Phase 1 — Weeks 1–2 (1 – 14 December 2026): Unblock the Team, Build Tier S

*Tier S = Step Execution, Pipeline Highlighting, Structured Error Diagnostics, Native API Explorer — the features that prove this is a real compiler/VM, not a GUI shell.*

| Week | Dates | Work |
|---|---|---|
| 1 | Dec 1 – 7 | **Unblock everyone first.** Arpita ships `StructuredError.hs`, the JSON symbol-table export, and stage-tracking flags (Lexing/Parsing/Semantic/Bytecode passed-or-failed) before anything else — the GUI and VM work below is built directly on this shape. In parallel, Aurabhri and Manshi scaffold the remaining four shared contracts above and set up the new test suites. All five contracts are frozen by end of week. |
| 2 | Dec 8 – 14 | **Parallel build.** Arpita wires structured errors into the Lexer, Parser, and Semantic Analyser, and writes the "why" text for 10–15 common error cases. Aurabhri implements Step / Run / Pause / Reset against the existing VM loop and exposes per-step `VMSnapshot`s; builds the event monitor (event → handler → instructions executed). Manshi builds the Source↔Token↔AST↔Bytecode highlighting sync, the error-visualisation UI (pointer + stage pipeline + "why" panel), the Native API Explorer UI, and the Compile/Run/Step/Reset controls. |

**🎯 Milestone V2-1 (14 Dec):** All four Tier S features exist and pass their own unit tests in isolation — none is yet wired through the three demo scenarios end-to-end.

---

## Phase 2 — Week 3 (15 – 21 December 2026): Tier S Integration

| Week | Dates | Work |
|---|---|---|
| 3 | Dec 15 – 21 | All three members wire Platformer Logic, Enemy AI, and Dialogue Tree through the new step-execution and highlighting pipeline end-to-end. Everyone cross-tests everyone else's integration points, since this is the same kind of coordination-heavy week Integration Week 13 was in v1. |

**🎯 Milestone V2-2 (21 Dec):** All three demos run through synchronised step-by-step execution, pipeline highlighting, and structured error diagnostics; the Native API Explorer is browsable in the GUI. **Tier S complete.**

> Weeks 4–5 below span the December holiday period. Agree explicitly as a team on reduced-availability days rather than letting the schedule silently slip — see Risks.

---

## Phase 3 — Weeks 4–5 (22 December 2026 – 4 January 2027): Tier A Parallel Build

*Tier A = Interactive AST Inspection, In-App Language Manual, Compilation Statistics, Dialogue/Enemy AI State Visualisation — high-value additions built once Tier S is stable.*

| Week | Dates | Work |
|---|---|---|
| 4 | Dec 22 – 28 | Arpita drafts the in-app language-manual content and refines semantic-analysis edge cases surfaced by Tier S testing. Aurabhri exposes Dialogue Tree / Enemy AI state and transitions through `StateTransition.hs` (data only, no visuals yet). Manshi builds the interactive/clickable AST node detail panel and starts the compilation-statistics display (line count, token count, AST node count, bytecode instruction count, compile time). |
| 5 | Dec 29 – Jan 4 | Arpita continues a compiler-robustness pass on edge cases from Tier S feedback. Aurabhri finalises the transition-data shape to support Manshi's visual rendering. Manshi renders the Dialogue Tree / Enemy AI state visualisation consuming Aurabhri's data layer, finishes the in-app language-manual panel using Arpita's content, and polishes the compilation-statistics display. |

**🎯 Milestone V2-3 (4 Jan):** All four Tier A features are built and individually functional, pending full integration.

---

## Phase 4 — Week 6 (5 – 11 January 2027): Tier A Integration & Polish

| Week | Dates | Work |
|---|---|---|
| 6 | Jan 5 – 11 | Wire all four Tier A features into the three demo scenarios; cross-test; polish the dialogue/AI visual rendering and language-manual navigation. |

**🎯 Milestone V2-4 (11 Jan):** Tier S and Tier A are fully integrated across all three demos.

---

## Phase 5 — Week 7 (12 – 18 January 2027): System, Integration, Regression & Stress Testing

Per SDS Section 5, extended to cover the v2 additions as well as the original three demos:

- **Malformed scripts at every stage** — verify the error pipeline surfaces the correct stage and message.
- **Deeply nested conditionals** — verify AST/Bytecode highlighting stays synchronised.
- **Stack-growth checks** — verify the step-execution view and VM debugger stay accurate under load.
- **Regression pass** — confirm the original three demos behave identically to their v1 baseline; v2's changes must not alter existing behaviour, only add to it.

Bugs surfaced during this week are fixed within the week; this is the tier that must not be shortened to make room for Tier B.

**🎯 Milestone V2-5 (18 Jan):** The v2 feature set (Tier S + Tier A) passes full regression and stress testing against both new and original functionality.

---

## Phase 6 — Week 8 (19 – 25 January 2027): Tier B, Final Polish, Documentation & Release

| Week | Dates | Work |
|---|---|---|
| 8 | Jan 19 – 25 | **If Week 7 finished on schedule:** Manshi attempts Tier B in priority order — tooltips/hover docs first, then the example gallery, then autocomplete (reusing the Native API Explorer's data from Tier S). **If Week 7 overran:** this week absorbs that overrun instead, and Tier B is deferred to a future release without guilt, exactly as the Sprint Plan's own "cut/defer" guidance allows. Regardless of which happens: run a final acceptance-test dry run with all three demos, update the SRS/SDS to reflect the as-built v2 system, and prepare the v2 demo/viva walkthrough. |

**🎯 Milestone V2-6 — FINAL (25 Jan):** **LumenScript Studio v2 concludes** with an application-ready feature set: step-by-step VM execution, synchronised Source–Token–AST–Bytecode highlighting, structured stage-aware error diagnostics, a native engine API explorer, interactive AST inspection, an in-app language manual, live compilation statistics, and Dialogue Tree / Enemy AI state visualisation — validated against the original three demos with no regressions, plus whatever Tier B polish time allowed.

---

## Feature → Owner → File Map

For quick reference during implementation, following the same file-level ownership style as the v1 roadmap:

| Feature | Tier | Owner | Primary File(s) |
|---|---|---|---|
| Structured error objects | S | Arpita | `Diagnostics/StructuredError.hs` (new); wired into `Lexer.hs`, `Parser.hs`, `Semantic/ErrorLog.hs` |
| Native/global symbol table export (JSON) | S | Arpita | `Semantic/SymbolTable.hs` (extended) |
| "Why" explanations (10–15 common errors) | S | Arpita | `Diagnostics/ErrorExplanations.hs` (new) |
| Step / Run / Pause / Reset engine | S | Aurabhri | `VM/StepControl.hs` (new); wraps `VM.hs` |
| Per-step VM state exposure | S | Aurabhri | `VMSnapshot` in `StepControl.hs`; consumed by `GUI/StackTraceViewer.hs` |
| Event monitor | S | Aurabhri | `VM/EventMonitor.hs` (new); consumes `VM/EventTrigger.hs` |
| Source–Token–AST–Bytecode highlight sync | S | Manshi | `GUI/PipelineHighlight.hs` (new); consumes `Token.hs`, `AST.hs`, `Instruction.hs` |
| Error visualisation UI | S | Manshi | `GUI/ErrorPanel.hs` (new); consumes `StructuredError.hs` |
| Native Engine API Explorer UI | S | Manshi | `GUI/ApiExplorer.hs` (new); consumes `HostBindings.hs` |
| Compile/Run/Step/Reset controls | S | Manshi | `GUI/EditorPanel.hs` (extended) |
| Interactive/clickable AST nodes | A | Manshi | `GUI/ASTViewer.hs` (extended) |
| In-app language manual | A | Arpita (content) / Manshi (panel) | `GUI/LanguageManual.hs` (new) |
| Compilation statistics | A | Manshi | `GUI/CompilationStats.hs` (new) |
| Dialogue Tree / Enemy AI state visualisation | A | Aurabhri (data) / Manshi (render) | `Simulation/StateTransition.hs` (new) → `GUI/SimulationPanel.hs` (extended) |
| Tooltips / hover docs | B | Manshi | GUI-wide, built last |
| Example gallery | B | Manshi | `GUI/ExampleGallery.hs` (new) |
| Autocomplete | B | Manshi | `GUI/EditorPanel.hs` (extended); consumes `ApiExplorer.hs` data |

---

## Summary Gantt (by week)

```
Week:            1  2  3  4  5  6  7  8
Unblock/Tier S   [████████]
Tier S Integr.            [████]
Tier A Build                    [████████]
Tier A Integr.                           [████]
Testing/Regr.                                  [████]
Tier B/Polish/Release                                 [████]
```

---

## Key risks to watch

- **December holiday period (Weeks 4–5):** these weeks fall over the winter break. Agree on realistic availability as a team up front rather than discovering the slip in Week 6.
- **Week 1's five shared contracts are the new highest-depended-on interfaces** in the project, the same role `Token.hs`/`AST.hs` played in v1. A shape change to `StructuredError.hs` or `VMSnapshot` after Week 1 cascades into rework across both the GUI and VM owners.
- **Regression risk:** Tier S and Tier A changes touch modules that already work correctly in v1 (`Lexer.hs`, `Parser.hs`, `Semantic/*`, `VM.hs`). Week 7's regression pass exists specifically to catch any unintended behaviour change to the original three demos — it is not optional scope.
- **Tier B is the buffer-absorbing tier, not a priority.** Per the Sprint Plan's own guidance, Week 8 must protect Milestone V2-5's testing scope before spending any time on tooltips, the example gallery, or autocomplete — shipping v2 without Tier B is a full success; shipping Tier B with an untested Tier S/A is not.
