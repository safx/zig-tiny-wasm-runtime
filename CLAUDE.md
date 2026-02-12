
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a WebAssembly interpreter written in Zig 0.15. It implements WebAssembly 1.0 core specification with selected 2.0 extensions, primarily SIMD operations. The test suite includes 237 test files from the spectec repository (WebAssembly 3.0 branch) that successfully parse and load. This educational project demonstrates understanding of WebAssembly fundamentals and key extension features.

**Important**: The test runner executes spec test assertions (assert_return, assert_trap, assert_invalid, etc.) against parsed and loaded modules. The 99.6% pass rate reflects actual execution correctness for implemented features. Remaining failures are due to unimplemented WebAssembly 3.0 features (tail calls, exception handling, GC types, typed references).

## Commands

### Build
```bash
# Build the main interpreter
zig build

# Build and run the interpreter
zig build run -- file.wasm -r function_name -a arg1

# Build the spec test runner
zig build spec_test
```

### Test
```bash
# Run all unit tests
zig build test

# Setup WebAssembly test suite from spectec (wasm-3.0 branch)
make setup-tests

# Run all WebAssembly spec tests
make test

# Run specific test file
python3 ./run_spectec_tests.py wasm_tests/test_name.wast

# Run with verbose output
python3 ./run_spectec_tests.py --verbose wasm_tests/

# Save assertion baseline for regression tracking
python3 ./run_spectec_tests.py wasm_tests/ --save-baseline baseline.json

# Compare current results against a saved baseline
python3 ./run_spectec_tests.py wasm_tests/ --compare baseline.json
```

### Development
```bash
# Format code
zig fmt src/

# Check for compilation errors without building
zig build-exe src/main.zig -O Debug
```

## Architecture

The codebase is organized into five main modules that build on each other:

1. **wasm-core** (`src/core/`): Defines WebAssembly types, instructions, and basic structures. This is the foundation that other modules depend on.

2. **wasm-decode** (`src/decode/`): Handles parsing WebAssembly binary format into the core structures. Includes the loader that manages imports/exports and module instantiation.

3. **wasm-text-decode** (`src/text_decode/`): Parses WebAssembly text format (.wast/.wat) into core structures. Provides lexer, parser, and module builder for text format support.

4. **wasm-validate** (`src/validate/`): Implements WebAssembly validation rules to ensure modules are well-formed before execution.

5. **wasm-runtime** (`src/runtime/`): The execution engine that interprets WebAssembly instructions. Contains the store (runtime state), instance management, and the main interpreter loop.

Key architectural decisions:
- Each module exports a root struct (e.g., `wasm_core`, `wasm_decode`) that contains all public APIs
- Uses Zig's allocator pattern throughout for memory management
- Validation is separate from decoding, following the WebAssembly spec structure
- The runtime uses a stack-based interpreter design

## Important Notes

- The project uses Zig 0.15.2 and may need updates for newer Zig versions
- When modifying instruction implementations, ensure both the decode and runtime modules are updated
- The test suite (237 test files from spectec wasm-3.0 branch) runs 61,857 assertions - always run `make test` after changes
- **Test coverage**: 61,604/61,857 (99.6%) assertions pass. Remaining 253 failures are due to unimplemented 3.0 features (tail calls, exception handling, GC types, typed references)
- Text format (.wast/.wat) files are handled by the spec_test binary, binary format (.wasm) by the main interpreter
- The project links with libc (uses C allocator) so platform-specific issues may arise
- Features like tail calls, exception handling, GC, and typed references are not implemented despite test files being present
