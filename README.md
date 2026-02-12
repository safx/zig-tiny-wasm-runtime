# Zig WebAssembly Interpreter

A tiny WebAssembly interpreter written in Zig. This interpreter implements WebAssembly 1.0 core features with selected 2.0 extensions, primarily focusing on SIMD operations.
This project is intended for personal understanding of the WebAssembly specification. Do not use in production environment.

## Requirements

- Zig 0.15.2
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

## WebAssembly Test Suite

This project uses test files from the [spectec](https://github.com/Wasm-DSL/spectec) repository (WebAssembly 3.0 branch).

**Important Note**: The current test runner parses and validates .wast files. Module loading and full test execution are partially implemented. Test results indicate parsing success and basic execution capability.

### Setup Test Suite

```bash
# Download and setup WebAssembly test files
make setup-tests

# This will:
# - Clone the spectec repository (wasm-3.0 branch)
# - Copy .wast test files to wasm_tests/
# - Exclude GC-related tests that require advanced runtime support
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

The codebase is organized into seven main modules:

### Core Modules

1. **wasm-core** (`src/core/`): WebAssembly types, instructions, and basic structures
2. **wasm-decode** (`src/decode/`): Binary format parsing and module loading
3. **wasm-text-decode** (`src/text_decode/`): Text format (.wast/.wat) parsing
4. **wasm-validate** (`src/validate/`): WebAssembly validation rules
5. **wasm-runtime** (`src/runtime/`): Execution engine and interpreter

### Spec Test Modules

6. **spec-types** (`src/spec-types/`): Pure type definitions for spec test commands and values
   - No runtime dependencies, only depends on wasm-core
   - Defines Command, Action, Value, Result types for test specifications
   - Includes FloatType for NaN handling (canonical/arithmetic)

7. **spec-test-errors** (`src/spec-test-errors/`): Error string mapping for spec tests
   - Maps error strings from test files to typed error enums
   - Safe fallback to OtherError for unknown strings (no panics)

8. **wasm-spec-test** (`src/spec_test/`): Test execution framework
   - Unified test runner for both .wast and .json formats
   - Executor module with value conversion and comparison logic
   - JSON and WAST readers for different test formats

### Dependency Graph

```
core
  ↓
spec-types (pure)
  ↓
text-decode → spec-test
  ↓
(decode, validate, runtime)
  ↓
spec-test-errors
  ↓
spec-test (executor + runners)
```

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

This interpreter implements **WebAssembly 1.0 core specification** with **selected 2.0 extensions**. The test suite shows **237/237 test files successfully parsed**, though full feature execution is limited to implemented instructions.

### ✅ **Fully Implemented Features**

#### **WebAssembly 1.0 Core**
- All basic numeric instructions (i32, i64, f32, f64)
- Control flow (block, loop, if, br, br_if, br_table, call, call_indirect)
- Memory operations (load, store with various sizes)
- Local and global variables
- Function calls and returns

#### **WebAssembly 2.0 Extensions**

1. **Vector/SIMD Instructions (Fixed-Width 128-bit)**
   - All standard SIMD operations (i8x16, i16x8, i32x4, i64x2, f32x4, f64x2)
   - Vector load/store operations with lane access
   - SIMD arithmetic, comparison, and bitwise operations
   - 236 standard SIMD instructions fully implemented

2. **Relaxed SIMD Instructions**
   - Non-deterministic SIMD operations for performance
   - Relaxed min/max, madd/nmadd operations
   - Relaxed lane selection and truncation
   - 21 relaxed SIMD instructions implemented

3. **Bulk Memory Operations**
   - `memory.copy` - Fast memory-to-memory copying
   - `memory.fill` - Memory initialization with byte values
   - `memory.init` - Initialize memory from passive data segments
   - `data.drop` - Drop passive data segments

4. **Reference Types (Basic)**
   - `funcref` and `externref` types
   - `ref.null`, `ref.func`, `ref.is_null` instructions
   - Multiple tables with reference types
   - Table operations: `table.get`, `table.set`, `table.init`, `table.copy`, `table.grow`, `table.size`, `table.fill`

5. **Non-Trapping Float-to-Int Conversions**
   - `i32.trunc_sat_f32_s/u`, `i32.trunc_sat_f64_s/u`
   - `i64.trunc_sat_f32_s/u`, `i64.trunc_sat_f64_s/u`

6. **Sign Extension Instructions**
   - `i32.extend8_s`, `i32.extend16_s`
   - `i64.extend8_s`, `i64.extend16_s`, `i64.extend32_s`

7. **Multi-Value Support**
   - Functions and blocks can return multiple values
   - Multi-value block types

8. **Memory64** (64-bit memory addressing)
   - Full u64 address handling for all memory operations
   - Dynamic address type selection (i32/i64) based on memory type
   - 100% pass rate on all memory64 spec tests

9. **Multi-Memory** (multiple memory instances per module)
   - Multiple memory addresses per module instance
   - `memory.copy` supports copying between different memories
   - All memory instructions accept memory index parameter

10. **Table64** (64-bit table addressing)
    - Dynamic address type (i32/i64) for all table operations
    - 100% pass rate on all table64 spec tests

11. **Extended Constant Expressions**
    - Arithmetic in constant expressions: `i32.add`, `i32.sub`, `i32.mul`
    - 64-bit arithmetic: `i64.add`, `i64.sub`, `i64.mul`
    - `global.get` of imported globals in initializer expressions

### 🔧 **Additional Capabilities**
- Text format (.wast/.wat) parsing and execution via spec test runner
- Binary format (.wasm) decoding and execution
- Module validation (types, control flow, stack usage)
- Import/export system with inter-module linking
- Memory bounds checking (traps on out-of-bounds access)

## Test Suite Status

- **237/237 test files** from [spectec](https://github.com/Wasm-DSL/spectec) successfully parse and load
- **61,604/61,857 (99.6%)** assertions pass across the test suite
- Test files cover WebAssembly 1.0, 2.0, and some 3.0 features

## Known Limitations

The following WebAssembly 3.0 features are **not implemented**:

- ❌ **Tail Calls** (`return_call`, `return_call_indirect`, `return_call_ref`)
- ❌ **Exception Handling** (`throw`, `throw_ref`, `try_table`, exception tags)
- ❌ **Garbage Collection** (struct, array, i31ref, and related GC instructions)
- ❌ **Extended Reference Types** (`br_on_null`, `br_on_non_null`, `call_ref`, `ref.as_non_null`)

These features can be parsed from .wast files but their instructions are not executed.

Additionally, the **reference type system** supports only `funcref` and `externref`. Non-nullable references (`ref func`), typed references (`ref null $t`, `ref $t`), and reference subtyping are not distinguished at the type level. This affects import matching for globals and tables that use these finer-grained reference types (21 `assert_unlinkable` failures in `linking.wast`).

## Contributing

This is an educational project for understanding WebAssembly internals. Feel free to explore and learn from the code structure.