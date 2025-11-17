
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a WebAssembly interpreter written in Zig 0.15. It implements WebAssembly 1.0 core specification with selected 2.0 extensions, primarily SIMD operations. The test suite includes 237 test files from the spectec repository (WebAssembly 3.0 branch) that successfully parse and load. This educational project demonstrates understanding of WebAssembly fundamentals and key extension features.

**Important**: The test runner validates parsing and module loading only, not complete feature execution. Passing tests indicate syntax compatibility rather than full implementation of all features present in test files.

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

# Run all WebAssembly spec tests (validates parsing/loading)
make test

# Run specific test file
python3 ./run_spectec_tests.py wasm_tests/test_name.wast

# Run with verbose output
python3 ./run_spectec_tests.py --verbose wasm_tests/
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
- The test suite (237 test files from spectec wasm-3.0 branch) validates parsing and loading - always run `make test` after changes
- **Test limitations**: The current test runner only validates that modules can be parsed and loaded, not that all instructions execute correctly
- Text format (.wast/.wat) files are handled by the spec_test binary, binary format (.wasm) by the main interpreter
- The project links with libc (uses C allocator) so platform-specific issues may arise
- Features like tail calls, exception handling, GC, multi-memory, and memory64 are not implemented despite test files being present
