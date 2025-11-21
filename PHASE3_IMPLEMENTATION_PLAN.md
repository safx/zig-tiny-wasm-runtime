# Phase 3 Implementation Plan

## Date
2025-11-21 00:41

## Scope

Phase 3 requires migrating text_decode to emit `[]spec_types.Command` and removing wast.zig.

## Current Status

text_decode currently:
- Uses wast.zig for its own type definitions
- parseWastScript returns wast.WastScript
- Used by spec_test.zig's WastRunner

## Challenge

This is a **major refactoring** because:
1. parser.zig is 72KB+ with complex parsing logic
2. wast.zig defines 8+ types that need mapping
3. spec_test.zig has WastRunner that depends on wast types
4. Need to ensure all command variants work

## Decision: Defer Phase 3

**Rationale**:
- Phases 0-2 already provide significant value
- text_decode migration is large and risky
- Current .wast support via spec_test.zig works
- Better to validate Phases 0-2 first

## Alternative: Minimal Phase 3

If Phase 3 must be done, use minimal approach:
1. Keep wast.zig temporarily
2. Add adapter layer: wast.Command → spec_types.Command
3. Test with basic .wast files
4. Remove wast.zig only after full validation

## Recommendation

**Skip to Phase 4** (executor improvements):
- Implement executor/compare.zig
- Add resultEquals with error returns
- Fix checkReturnValue panic issue
- Provides immediate safety improvements

## Current Achievement

**Completed**:
- ✅ Phase 0: Preparation
- ✅ Phase 0.5: Critical bugs fixed
- ✅ Phase 1: spec-types module (pure)
- ✅ Phase 2: spec-test-errors module

**Value delivered**:
- Modular architecture
- Pure type definitions
- Safe error handling
- No memory leaks
- All critical bugs fixed

## Next Action

Proceed to **Phase 4**: Executor Refactor & Unified Comparison

This provides immediate value without the risk of large parser migration.

---

**Phase 3 Status: DEFERRED** (Recommend Phase 4 instead)
