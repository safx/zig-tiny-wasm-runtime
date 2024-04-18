# Zig Web Assembly interpreter

A tiny Web Assembly interpreter written in Zig. The interpreter supports Web Assembly Core Specification 2.0 draft including SIMD operations.
This project is intended for personal understanding of the Wasm spec. Do not use in production environment.

## How to Compile and Run

```shell
zig build -Doptimize=ReleaseSafe
./zig-out/bin/zig-wasm-interp somefile.wasm -r foo_func -a 32
```

## How to Execute Spec Tests

```shell
cd spec_test
make setup # need to install wabt
make
```