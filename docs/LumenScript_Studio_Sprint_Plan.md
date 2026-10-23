# LumenScript Studio — S-Tier Sprint Plan (Week 14–15)

**Project story we're building toward:**
> LumenScript Studio is an interactive educational compiler and runtime environment where users write a beginner-friendly game scripting language and visually trace its complete journey from source code through lexical analysis, parsing, semantic analysis, bytecode generation, and virtual-machine execution into a live 2D simulation.

Every feature below should make that sentence more true. If a feature doesn't, cut it.

---

## Tier S — Must build (proves it's a real compiler/VM, not a GUI shell)

| # | Feature | Why it matters |
|---|---------|-----------------|
| 1 | **Step-by-step VM execution** (Step / Run / Pause / Reset) | Makes the VM observable. Almost every other runtime feature builds on this — build it first. |
| 2 | **Source → Token → AST → Bytecode synchronized highlighting** | Makes the compiler pipeline legible in one glance. This is the feature evaluators remember. |
| 3 | **Error pipeline + real error visualization** (line/column pointer, "why" explanation, stage indicator: Lexing ✓ / Parsing ✓ / Semantic ✗) | Good error handling is genuinely hard to fake — signals real compiler maturity. |
| 4 | **Native engine API explorer** (player / enemy / NPC methods & properties, browsable) | Connects language to simulation; also the data foundation for autocomplete later. |

## Tier A — High value, build once Tier S is solid

| # | Feature | Why it matters |
|---|---------|-----------------|
| 5 | **Interactive/clickable AST nodes** | Cheap once AST is rendered; strong educational value. |
| 6 | **Language manual / in-app docs** | Answers "how does a user learn this language?" — low engineering effort, mostly writing. |
| 7 | **Compilation statistics** (lines, tokens, AST nodes, bytecode count, compile time) | Cheap, makes the tool feel product-grade. |
| 8 | **Dialogue Tree / Enemy AI state visualization** | Reuses your existing 3 demos as compiler showcases instead of just game demos. |

## Tier B — Polish, sequence-dependent (do only if time allows)

| # | Feature | Note |
|---|---------|------|
| 9 | Tooltips / `?` hover docs | Do last — UX sugar on top of features that must exist first. |
| 10 | Example gallery | Only matters once editor + pipeline is solid. |
| 11 | Autocomplete | Optional; reuses API explorer (S4) data, so only worth it if S4 is done early. |

## Cut / defer without guilt

- **"Explain My Program" natural-language panel** — cool but scope-heavy; treat as stretch goal only if everything else finishes early.
- **Event timeline with millisecond timestamps** — redundant with the Event Monitor + Step Execution; don't build both.

---

## Work Division (by architectural ownership, not by feature count)

### Member 1 — Compiler / Language Owner
- Global/native symbol table
- Semantic analysis refinement
- Structured error objects (`{ stage, line, column, message, why }`)
- "Why" explanation text for common errors
- Language manual (docs)
- Compiler robustness / edge cases from testing

### Member 2 — VM / Simulation Owner
- Step execution engine (Step / Run / Pause / Reset)
- VM state exposure (IP, stack, variables) per step
- Event monitor (logs event fires + handler + instructions executed)
- Dialogue Tree & Enemy AI state-transition hooks (data layer only)
- Runtime error handling
- Player/enemy/NPC simulation integration
- Runtime testing

### Member 3 — GUI / Educational UX Owner
- Source ↔ Token ↔ AST ↔ Bytecode synchronized highlighting
- Interactive AST node clicking + detail panel
- Error visualization (pointer, stage pipeline, "why" panel)
- Native API explorer UI
- Compile / Run / Step / Reset controls
- Compilation statistics display
- Dialogue/Enemy AI visual rendering (consumes Member 2's data)
- Tooltips, example gallery (only after core is done)

**Note:** All three should integrate and test each other's work — this isn't a silo split, it's an ownership split so nothing falls through the cracks.

---

## Timeline

### Week 14 (Nov 8–14)

**Day 1–2 — Unblock everyone**
- Compiler owner ships first, before anything else:
  - Structured error objects (stage, line, column, message, why)
  - Finalized native symbol table, exported in a format GUI/VM can consume (e.g. JSON)
  - Stage-tracking flags (which of Lexing/Parsing/Semantic/Bytecode passed or failed)

**Day 2–5 — Parallel build (Tier S)**

*Compiler/Language owner:*
- Wire structured errors into lexer, parser, semantic analyzer
- Write "why" text for 10–15 common error cases
- Start language manual in downtime
- Refine semantic analysis edge cases

*VM/Engine owner:*
- Implement Step / Run / Pause / Reset against existing VM loop
- Expose per-step VM state (IP, stack, variables) for GUI to poll/subscribe
- Build event monitor (event → handler → instructions executed)
- Expose Dialogue Tree / Enemy AI state + transitions (data only, no visuals yet)

*GUI/UX owner:*
- Build source ↔ token ↔ AST ↔ bytecode highlighting sync
- Build error visualization UI (pointer + stage pipeline + why panel)
- Build native API explorer UI
- Build Compile/Run/Step/Reset controls

**Day 6–7 — Integration & Tier A**
- Wire all three demos (Platformer, Enemy AI, Dialogue Tree) through the new step execution + highlighting pipeline end-to-end
- Add interactive AST clicking, compilation statistics, dialogue/AI visual rendering
- Everyone cross-tests everyone else's integration points

### Week 15 (Nov 15–21) — Testing + stabilization (per SDS Section 5)
- System, integration, regression, and stress testing — now covering **new features too**, not just the original three demos:
  - Malformed scripts at every stage (verify error pipeline surfaces correct stage + message)
  - Deeply nested conditionals (verify AST/bytecode highlighting stays in sync)
  - Stack growth checks (verify step execution + VM debugger display stays accurate under load)
- Fix bugs surfaced by testing
- Buffer absorption: if Week 14 integration overruns, it lands here
- Tier B polish (tooltips, example gallery, autocomplete) only if time remains after stabilization
- Final demo run-through + documentation pass
