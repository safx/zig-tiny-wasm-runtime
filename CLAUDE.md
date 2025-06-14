
# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project Overview

This is a WebAssembly interpreter written in Zig 0.14. It implements the WebAssembly Core Specification 2.0 draft (including SIMD) as an educational project for understanding the WebAssembly specification.

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

# Run WebAssembly spec tests (requires wabt installed)
./download_spec_test.sh  # First time only to download test files
zig build spec_test
./zig-out/bin/spec_test spec_test/test_name.json
```

### Development
```bash
# Format code
zig fmt src/

# Check for compilation errors without building
zig build-exe src/main.zig -O Debug
```

## Architecture

The codebase is organized into four main modules that build on each other:

1. **wasm-core** (`src/core/`): Defines WebAssembly types, instructions, and basic structures. This is the foundation that other modules depend on.

2. **wasm-decode** (`src/decode/`): Handles parsing WebAssembly binary format into the core structures. Includes the loader that manages imports/exports and module instantiation.

3. **wasm-validate** (`src/validate/`): Implements WebAssembly validation rules to ensure modules are well-formed before execution.

4. **wasm-runtime** (`src/runtime/`): The execution engine that interprets WebAssembly instructions. Contains the store (runtime state), instance management, and the main interpreter loop.

Key architectural decisions:
- Each module exports a root struct (e.g., `wasm_core`, `wasm_decode`) that contains all public APIs
- Uses Zig's allocator pattern throughout for memory management
- Validation is separate from decoding, following the WebAssembly spec structure
- The runtime uses a stack-based interpreter design

## Important Notes

- The project uses Zig 0.14 and may need updates for newer Zig versions
- When modifying instruction implementations, ensure both the decode and runtime modules are updated
- The spec test suite is the primary way to verify correctness - always run relevant spec tests after changes
- The project links with libc (uses C allocator) so platform-specific issues may arise

## **MOST IMPORTANT RULES**: Process for adding new rules

When you receive an instruction from a user that you think needs to be addressed always, not just once, follow these steps:

1. ask: “Do you want to make this a standard rule?”
2. If the answer is YES, the rule is added to the `CLAUDE.md` file and is always applied as a standard rule from then on.

This process is used to continuously improve the rules of the project.

## Critical Rules - DO NOT VIOLATE

- **NEVER create mock data or simplified components** unless explicitly told to do so
- **NEVER replace existing complex components with simplified versions** - always fix the actual problem
- **ALWAYS work with the existing codebase** - do not create new simplified alternatives
- **ALWAYS find and fix the root cause** of issues instead of creating workarounds
- When debugging issues, focus on fixing the existing implementation, not replacing it
- When something doesn't work, debug and fix it - don't start over with a simple version
