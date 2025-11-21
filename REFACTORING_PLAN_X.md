# REFACTORING_PLAN_X.md

> Date: 2025-11-20
> Goal: Complete, safe separation of spec_types (pure typed AST) and spec_test_errors (error mapping) while improving correctness (NaN/vector, error handling), memory ownership, and incremental migration of text_decode. This supersedes previous plans.

## 0. Guiding Principles
- Purity: `spec_types` depends only on `wasm-core` (and will drop even that import unless needed).
- Safety: No `unreachable` for external data paths (parsing / error string mapping).
- Determinism: Float / SIMD / NaN comparison semantics explicitly defined.
- Incremental migration: Direct parser replacement (no dual-output; legacy comparison dropped).
- Ownership clarity: Caller-managed slice lifetimes (no custom wrapper).
- Test-first for conversions and comparisons; failures return typed errors not panics except truly unrecoverable internal invariants.

## 1. Architectural Overview (Target)
```
core
  ↓
spec_types (pure: Value/Result/Action/Command + helpers)
  ↓
text_decode (parses → []spec_types.Command)

(decode, validate, runtime)
  ↓
spec_test_errors (string ↔ enum mapping with safe fallbacks)
  ↓
spec_test (executor + runners)
```

### 1.1 Module Contents
- spec_types:
  - command.zig: pure data unions; float bit conversion helpers kept as inline static methods on Value.
  - v128 representation: store as raw i128 (no runtime V128 reuse to avoid dependency); lane extraction happens in executor.
  - utils: none (no separate float_utils/nan files; keep minimal surface).
  - test.zig: exhaustive round-trip & property tests.
- spec_test_errors:
  - errors.zig: simple linear string → enum mapping (no binary search; <100 entries acceptable).
  - errors.zig: API returning `!DecodeError` etc (error union on unknown string) with fallback `.OtherError` variant (never panic).
- executor:
  - value_convert.zig: `toRuntimeValue`, `toSpecResult` (now includes vec_f32/vec_f64 lanes) and unified `resultEquals` used everywhere.
- text_decode:
  - parser.zig directly outputs `[]spec_types.Command`.
  - wast.zig removed in Phase 3.

## 2. Risks & Mitigations
| Risk | Mitigation |
|------|------------|
| NaN comparison incorrect (current OR bug, bit tests partial) | Centralize comparison in executor.resultEquals; use canonical pattern & arithmetic detection spec: exponent all 1s, quiet bit set (f32 bit 22, f64 bit 51), non-canonical mantissa ≠ canonical; tests for edge cases. |
| vec_f32/vec_f64 lane logic OR vs AND bug | Replace OR accumulation with lane-by-lane AND; failing lane logs diff. |
| Unknown error strings causing panic | Replace unreachable with error union; add `error.UnknownErrorString` surfaced in verbose mode only. |
| Parser migration regressions | Single direct migration; rely on targeted command coverage tests (no dual diff). |
| Memory leaks for Command arrays | Caller-managed slices; document allocator ownership; avoid custom OwnedSlice wrapper. |
| v128 lane extraction mismatch | Keep i128 storage; implement lane extraction helpers in executor; test lanes. |
| f32/f64 conversion scattered | Keep inline bitCast helpers on Value; reuse directly. |
| Module name null handling ambiguity | Explicit semantics: `null` = last successfully instantiated non-spectest module; executor stores pointer to current module; update on `.module` commands. |

## 3. Detailed Phases

### Phase 0 (Preparation)
Checklist:
- `zig fmt --check src/` passes.
- Baseline tests `zig build test` & `make test` recorded (store counts for later comparison).
- Create branch `refactor/spec-types-vX` & tag `phase0-start`.
Rollback: `git reset --hard phase0-start`.

### Phase 1 (Introduce spec_types Pure Module)
Steps:
1. Create `src/spec-types/` with files: `command.zig`, `mod.zig`, `test.zig` (no v128/float_utils/nan separate files).
2. Remove any unused imports (no runtime/decode/validate). Keep only `std` & optionally `wasm-core` if value tags rely on its enums; otherwise supply local enum.
3. Implement Value with tags: i32,i64,f32(u32 bits),f64(u64 bits),v128(i128 bits),func_ref(?FuncAddr),extern_ref(?ExternAddr).
4. Result mirrors runtime + FloatType + vec_f32/vec_f64 (arrays of FloatType).
5. (NaN helpers deferred) Provide NaN classification later in executor (Phase 4).
6. Tests: round-trip bit ↔ float, deterministic fuzz: 512 pseudo-random u32/u64 patterns (LCG) verify bitCast round-trip.
7. Update build.zig adding spec-types BEFORE text_decode but not referenced yet by other modules.
Success: build passes; spec-types tests pass; no additional module broken.
Rollback: remove directory + revert build.zig.

### Phase 2 (Introduce spec_test_errors Safe Mapping)
Steps:
1. Create `src/spec-test-errors/` with `errors.zig`, `mod.zig`, `test.zig` (no errors_table).
2. Implement linear if/else or switch-based string mapping (simplicity over perf).
3. Return `error.UnknownErrorString` on miss; fallback variant mapping optional.
4. Replace unreachable with error union strategy.
5. Add tests for known strings + unknown string scenario.
6. Update build.zig to include spec-test-errors.
Success: All tests including Phase 1 unaffected; unknown mapping test returns expected error.
Rollback: revert additions.

### Phase 3 (Direct Parser Migration)
Steps:
1. Replace parser to emit `[]spec_types.Command` directly (no dual legacy comparison; legacy deemed low value).
2. Remove `wast.zig` after new parser compiles and basic sample `.wast` files parse.
3. Implement missing command coverage (assert_malformed, assert_exhaustion, assert_unlinkable, assert_uninstantiable, module_quote) with stub or skip logic documented.
4. Add tests `text_decode/test_commands.zig` verifying representative commands map to expected tags / fields.
Success: All sample commands parse; no runtime panics; test coverage for each command variant.
Rollback: Restore previous parser from tag `phase2-end`.

### Phase 4 (Executor Refactor & Unified Comparison)
Steps:
1. Add `src/spec_test/executor/` directory: `value_convert.zig`, `compare.zig`.
2. Implement `resultEquals(expected: spec_types.Result, runtime: runtime.types.Value) bool` (single source of truth).
2.5 Implement NaN classification functions in compare.zig: `isCanonicalNanF32/64`, `isArithmeticNanF32/64`. 
3. Replace test_runner uses of previous checkReturnValue & inline NaN functions with executor.compare.
4. Ensure vec lane comparison uses AND semantics and logs first failing lane.
5. Introduce error types: `error.ResultLengthMismatch`, `error.ValueMismatch` returned instead of panics.
6. Update tests accordingly.
6.5 Add NaN classification tests: canonical/arithmetic detection edge cases.
Success: Executor tests pass; no panics on mismatch—errors returned.
Rollback: revert executor directory & test_runner changes.

### Phase 5 (Spec Test Runner Restructure)
Steps:
1. Replace direct use of legacy types with spec-types + conversions; remove old `types.zig` & `errors.zig`.
2. Use plain slices for command arrays; caller frees; no wrapper helper.
3. module_quote: skip (tag only, no stored text).
4. Update test_runner to handle error union returns; verbose prints on mismatch.
Success: `zig build spec_test` passes; old files removed.
Rollback: restore from tag `phase5-start`.

### Phase 6 (Entry Simplification)
Steps:
1. Simplify `spec_test.zig` main; remove WastRunner; wire new runner.
2. Re-run full tests.
Success: CLI works with existing flags; no legacy diff functionality required.
Rollback: restore earlier main.

### Phase 7 (Integration, Clean-up & Finalization)
Steps:
1. Remove any remaining legacy artifacts & duplicated NaN helpers.
2. Run full test matrix: unit, spec, fuzz subset.
3. Verify dependency graph via script: grep import patterns.
4. Final formatting & documentation updates (README: architecture section, spec_types explanation).
5. Tag `refactor-complete-vX`.
Success: Baseline spec tests unchanged (237 passing), improved internal tests added.
Rollback: tag-based reset.

## 4. Testing Strategy (Expanded)
- Unit: Per module test file + aggregated `zig build test`.

- Property: Deterministic fuzz for float conversions (512 iterations).
- SIMD: Lane extraction tests for multiple element widths (u8,u16,u32,f32).
- Error mapping: Known vs unknown string scenarios; ensures no panic.
- Performance sanity: Ensure no dramatic slowdown (manual spot check; no fixed % threshold).

## 5. Error Handling Conventions
- External data (text/json strings) never triggers `unreachable`; use error union.
- Unknown error string returns `error.UnknownErrorString`; executor treats it as mismatch (logs & fails test) rather than crash.
- Internal invariants (e.g. impossible tag) may still use `unreachable` inside private helpers.

## 6. NaN & SIMD Semantics Reference
- Canonical NaN (f32): bits `0x7fc00000`; (f64): `0x7ff8000000000000`.
- Arithmetic NaN: exponent all 1s, quiet bit set, mantissa ≠ canonical pattern.
- Vector NaNs: Each lane independently compared; all lanes must satisfy respective expectation.

## 7. Ownership & Memory
- Commands returned as plain slices; caller frees via allocator (no wrapper).
- Arena use limited to short-lived parsing; executor converts to runtime values with separate allocations freed after use.
- Prohibit mixing arena slices in long-lived store structures.

## 8. Logging & Verbosity
- Verbose level ≥2: prints detailed command parsing and comparison info.
- Verbose level ≥1: prints each command & comparison summary.
- Level 0: silent except errors.

## 9. CI / Automation Hooks (Optional if CI exists)
- Add script `scripts/verify_refactor.sh` to run sequence: build, test, diff, spec tests, grep dependency.

## 10. Acceptance Criteria Summary
- Purity: `spec_types` imports exclude runtime/decode/validate.
- Safety: No panic/unreachable on external strings/instructions parse.
- Correctness: NaN & vector comparisons validated; lane logic fixed.
- Completeness: All required `.wast` commands supported or explicitly skipped with documented reason (module_quote skipped).
- Performance: No significant observed slowdown vs baseline (qualitative).
- Documentation: README updated.

## 11. Rollback Strategy Enhancements
- Tag at each phase start: `phaseN-start`.
- To revert a phase: `git reset --hard phaseN-start`.
- To revert a single file: `git restore --source=phaseN-start path/to/file`.
- Emergency full rollback: `git reset --hard phase0-start && git branch -D refactor/spec-types-vX`.

## 12. Open TODO / Post-Refactor Items
- Implement full support for module binary/instance forms (currently skipped).
- Extend error mapping tables with all remaining FIXME cases from legacy.
- Introduce structured diff tool for commands (pretty-print). 
- Potential future: unify runtime Value and spec-types Value if execution of spec scripts becomes richer.

### Known Issues (Pre-Refactor)
- ⚠️ **register command bug**: `test_runner.zig` ignores `Command.register.name` and always registers `current_module`. Should select module by name if provided. Fix in Phase 5.
- ⚠️ **text_decode implementation gaps**: Many `assert_*` commands not implemented in parser. Phase 3 will document which commands are implemented vs. stubbed/skipped.
- ⚠️ **checkReturnValue still panics**: Current implementation panics on mismatch instead of returning typed errors. Phase 4 will implement `resultEquals` with error-returning comparison layer.

## 13. Task Matrix (Checklist)
(See Phases for detailed boxes)
- [x] Phase 0
- [x] Phase 1
- [x] Phase 2
- [x] Phase 3
- [x] Phase 4
- [x] Phase 5
- [x] Phase 6
- [x] Phase 7
- [x] Documentation Update

## 14. Rationale Improvements vs Previous Plans
- Eliminated OR bug in vector comparisons.
- Centralized comparison logic to avoid divergence.
- Removed unnecessary dual parser safety net (simplified migration).
- Removed unsafe unreachable paths for external data.
- Formalized ownership & memory semantics.
- Simplified error mapping to linear scan for maintainability.

## 15. Glossary
- Canonical NaN: Defined by WebAssembly spec canonical pattern.
- Arithmetic NaN: Non-canonical quiet NaN used in arithmetic propagation tests.

---
End of Plan.
