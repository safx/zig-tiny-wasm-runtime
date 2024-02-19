const std = @import("std");
pub const DecodeError = @import("wasm-decode").Error;
pub const ValidationError = @import("wasm-validate").Error;
pub const RuntimeError = @import("wasm-runtime").Error;

pub fn runtimeErrorFromString(str: []const u8) RuntimeError {
    const E = RuntimeError;
    if (strcmp(str, "indirect call type mismatch")) return E.IndirectCallTypeMismatch;
    if (strcmp(str, "integer divide by zero")) return E.IntegerDivideByZero;
    if (strcmp(str, "integer overflow")) return E.IntegerOverflow;
    if (strcmp(str, "invalid conversion to integer")) return E.InvalidConversionToInteger;
    if (strcmp(str, "out of bounds memory access")) return E.OutOfBoundsMemoryAccess;
    if (strcmp(str, "out of bounds table access")) return E.OutOfBoundsTableAccess;
    if (strcmp(str, "undefined element")) return E.UndefinedElement;
    if (strcmp(str, "uninitialized element") or strcmp(str, "uninitialized element 2")) return E.UninitializedElement;
    if (strcmp(str, "unreachable")) return E.Unreachable;

    std.debug.print("? Unknown runtime error \"{s}\"\n", .{str});
    unreachable;
}

pub fn decodeErrorFromString(str: []const u8) DecodeError {
    const E = DecodeError;
    if (strcmp(str, "incompatible import type")) return E.IncompatibleImportType;
    if (strcmp(str, "unknown import")) return E.UnknownImport;

    std.debug.print("? Unknown decode error \"{s}\"\n", .{str});
    unreachable;
}

pub fn validationErrorFromString(str: []const u8) ValidationError {
    const E = ValidationError;

    if (strcmp(str, "alignment must not be larger than natural")) return E.NegativeNumberAlignment;
    if (strcmp(str, "constant expression required")) return E.ConstantExpressionRequired;
    if (strcmp(str, "duplicate export name")) return E.DuplicateExportName;
    if (strcmp(str, "global is immutable")) return E.ImmutableGlobal;
    if (strcmp(str, "invalid result arity")) return E.InvalidResultArity;
    if (strcmp(str, "memory size must be at most 65536 pages (4GiB)")) return E.MemoryMaxSpecSizeExceeded;
    if (strcmp(str, "multiple memories")) return E.MultipleMemories;
    if (strcmp(str, "size minimum must not be greater than maximum")) return E.MemorySizeExceeded;
    if (strcmp(str, "start function")) return E.StartFunction;
    if (strcmp(str, "type mismatch")) return E.TypeMismatch;
    if (strcmp(str, "undeclared function reference")) return E.UndeclaredFunctionReference;
    if (strcmp(str, "unknown data segment") or strcmp(str, "unknown data segment 1")) return E.UnknownDataSegment;
    if (strcmp(str, "unknown elem segment 0") or strcmp(str, "unknown elem segment 1") or strcmp(str, "unknown elem segment 4")) return E.UnknownElementSegment;
    if (strcmp(str, "unknown function") or strcmp(str, "unknown function 7")) return E.UnknownFunction;
    if (strcmp(str, "unknown global") or strcmp(str, "unknown global 0") or strcmp(str, "unknown global 1")) return E.UnknownGlobal;
    if (strcmp(str, "unknown function")) return E.UnknownFunction;
    if (strcmp(str, "unknown label")) return E.UnknownLabel;
    if (strcmp(str, "unknown local")) return E.UnknownLocal;
    if (strcmp(str, "unknown memory") or strcmp(str, "unknown memory 0") or strcmp(str, "unknown memory 1")) return E.UnknownMemory;
    if (strcmp(str, "unknown table") or strcmp(str, "unknown table 0")) return E.UnknownTable;
    if (strcmp(str, "unknown type")) return E.UnknownType;

    std.debug.print("? Unknown validation error \"{s}\" {any}\n", .{ str, str });
    unreachable;
}

inline fn strcmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
