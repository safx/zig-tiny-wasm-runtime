# Zig WebAssembly Interpreter

A tiny WebAssembly interpreter written in Zig. The interpreter supports WebAssembly Core Specification 2.0 draft including SIMD operations.
This project is intended for personal understanding of the WebAssembly specification. Do not use in production environment.

## Requirements

- Zig 0.14.1
- Python 3.x (for test runner)
- Git (for fetching test suite)

## Build

```bash
# Build the main interpreter
zig build

# Build the spec test runner
zig build spec_test

# Or use the Makefile
make build
make build-spec-test
```

## Usage

### Running WebAssembly Files

```bash
# Run a WebAssembly binary file
./zig-out/bin/zig-wasm-interp somefile.wasm -r function_name -a 32

# Run with verbose output
./zig-out/bin/zig-wasm-interp somefile.wasm -v -r function_name -a i32:42 -a f64:3.14
```

### Running Text Format Files (.wast/.wat)

```bash
# Use the spec test runner for text format files
./zig-out/bin/spec_test test.wast -v
```

## WebAssembly 2.0 Test Suite

This project now supports the new WebAssembly 2.0 test suite from [spectec](https://github.com/Wasm-DSL/spectec).

### Setup Test Suite

```bash
# Download and setup WebAssembly 2.0 test files
make setup-tests

# This will:
# - Clone the spectec repository
# - Copy compatible .wast test files to wasm_tests/
# - Filter out unsupported advanced features
```

### Run Tests

```bash
# Run all WebAssembly spec tests
make test

# Run specific test file
python3 ./run_spectec_tests.py wasm_tests/address.wast

# Run with verbose output
python3 ./run_spectec_tests.py --verbose wasm_tests/

# Stop on first failure
python3 ./run_spectec_tests.py --failfast wasm_tests/
```

## Architecture

The codebase is organized into four main modules:

1. **wasm-core** (`src/core/`): WebAssembly types, instructions, and basic structures
2. **wasm-decode** (`src/decode/`): Binary format parsing and module loading
3. **wasm-text-decode** (`src/text_decode/`): Text format (.wast/.wat) parsing
4. **wasm-validate** (`src/validate/`): WebAssembly validation rules
5. **wasm-runtime** (`src/runtime/`): Execution engine and interpreter

## Commands Reference

```bash
# Development
make help              # Show available commands
make build             # Build the interpreter
make build-spec-test   # Build spec test runner
make clean             # Clean all build artifacts

# Testing
make setup-spectec     # Clone/update spectec repository
make setup-tests       # Setup WebAssembly 2.0 test files
make test              # Run WebAssembly spec tests
make clean-tests       # Remove test files

# Manual testing
zig build test         # Run unit tests
zig fmt src/           # Format source code
```

## Supported Features

This interpreter provides **comprehensive WebAssembly 2.0 support** with **223/223 test cases passing** (100% pass rate):

### ✅ **WebAssembly 2.0 Official Specification Features**

1. **Vector/SIMD Instructions** 
   - All 236 new SIMD instructions
   - Standard SIMD operations (i8x16, i16x8, i32x4, f32x4, etc.)

2. **Bulk Memory Instructions**
   - Fast memory and table copying/initialization
   - `memory.copy`, `memory.fill`, `memory.init`, `data.drop`

3. **Multi-Value Results**
   - Functions and blocks can return multiple values
   - Block instructions with inputs

4. **Reference Types**
   - First-class references to functions (`funcref`)
   - External object pointers (`externref`)
   - Multiple tables of different types
   - `ref.null`, `ref.func`, `ref.is_null`, `ref.as_non_null`
   - Reference-based branching (`br_on_null`, `br_on_non_null`)

5. **Non-Trapping Conversions**
   - Safe float-to-integer conversions

6. **Sign Extension Instructions**
   - Direct width extension for signed integer values

### 🚀 **Additional Advanced Features (Post-2.0 Proposals)**

1. **Relaxed SIMD Extensions**
   - Enhanced SIMD operations (relaxed_min_max, relaxed_dot_product, etc.)
   - Performance-optimized vector operations

2. **Multi-Memory Support**
   - Multiple memory instances in a single module
   - Memory-indexed operations

3. **Tail Calls**
   - `return_call` - direct tail calls
   - `return_call_indirect` - indirect tail calls  
   - `return_call_ref` - reference-based tail calls

4. **Advanced Type System**
   - Type equivalence and canonicalization
   - Recursive types
   - Function reference types

5. **Exception Handling**
   - `throw` and `throw_ref` instructions
   - Exception tags and type system
   - `try_table` structured exception handling

6. **Memory64**
   - 64-bit memory addressing support
   - Extended memory operations

### 🔧 **Additional Features**
- Text format (.wast/.wat) parsing and execution
- Binary format (.wasm) execution
- Complete validation according to WebAssembly spec
- Import/export system
- Memory operations and bounds checking

## Test Coverage

- **223 WebAssembly 2.0 test cases** from the official [spectec](https://github.com/Wasm-DSL/spectec) test suite
- **100% pass rate** across all supported features
- Continuous testing against the latest WebAssembly specifications

## Limitations

Only one advanced WebAssembly feature is not yet supported:
- **Garbage Collection (GC)** - Requires advanced runtime support with custom heap management

## Contributing

This is an educational project for understanding WebAssembly internals. Feel free to explore and learn from the code structure.