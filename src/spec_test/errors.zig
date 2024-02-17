const std = @import("std");
const decode = @import("wasm-decode");
const runtime = @import("wasm-runtime");
pub const Error = decode.Error || runtime.Error;

pub fn errorFromString(str: []const u8) Error {
    const E = Error;
    if (strcmp(str, "integer divide by zero")) {
        return E.IntegerDivideByZero;
    } else if (strcmp(str, "integer overflow")) {
        return E.IntegerOverflow;
    } else if (strcmp(str, "out of bounds memory access")) {
        return E.OutOfBoundsMemoryAccess;
    } else if (strcmp(str, "out of bounds table access")) {
        return E.OutOfBoundsTableAccess;
    } else if (strcmp(str, "undefined element")) {
        return E.UndefinedElement;
    } else if (strcmp(str, "uninitialized element")) {
        return E.UninitializedElement;
    } else if (strcmp(str, "uninitialized element 2")) {
        return E.UninitializedElement;
    } else if (strcmp(str, "indirect call type mismatch")) {
        return E.IndirectCallTypeMismatch;
    } else if (strcmp(str, "invalid conversion to integer")) {
        return E.InvalidConversionToInteger;
    } else if (strcmp(str, "unreachable")) {
        return E.Unreachable;
    } else {
        std.debug.print("? Unknown error \"{s}\"\n", .{str});
        unreachable;
    }
}

inline fn strcmp(a: []const u8, b: []const u8) bool {
    return std.mem.eql(u8, a, b);
}
