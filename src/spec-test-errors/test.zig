const std = @import("std");
const errors = @import("./errors.zig");

test "spec-test-errors: decodeErrorFromString" {
    const err1 = errors.decodeErrorFromString("magic header not detected");
    try std.testing.expect(err1 == errors.DecodeError.MagicHeaderNotDetected);
    
    const err2 = errors.decodeErrorFromString("integer too large");
    try std.testing.expect(err2 == errors.DecodeError.IntegerTooLarge);
    
    // Unknown string returns OtherError
    const err3 = errors.decodeErrorFromString("unknown error string");
    try std.testing.expect(err3 == errors.DecodeError.OtherError);
}

test "spec-test-errors: validationErrorFromString" {
    const err1 = errors.validationErrorFromString("type mismatch");
    try std.testing.expect(err1 == errors.ValidationError.TypeMismatch);
    
    const err2 = errors.validationErrorFromString("unknown function");
    try std.testing.expect(err2 == errors.ValidationError.UnknownFunction);
    
    // Unknown string returns OtherError
    const err3 = errors.validationErrorFromString("unknown error string");
    try std.testing.expect(err3 == errors.ValidationError.OtherError);
}

test "spec-test-errors: runtimeErrorFromString" {
    const err1 = errors.runtimeErrorFromString("unreachable");
    try std.testing.expect(err1 == errors.RuntimeError.Unreachable);
    
    const err2 = errors.runtimeErrorFromString("integer divide by zero");
    try std.testing.expect(err2 == errors.RuntimeError.IntegerDivideByZero);
    
    // Unknown string returns OtherError
    const err3 = errors.runtimeErrorFromString("unknown error string");
    try std.testing.expect(err3 == errors.RuntimeError.OtherError);
}

test "spec-test-errors: linkErrorFromString" {
    const err1 = errors.linkErrorFromString("incompatible import type");
    try std.testing.expect(err1 == errors.RuntimeError.IncompatibleImportType);
    
    // Unknown string returns OtherError
    const err2 = errors.linkErrorFromString("unknown error string");
    try std.testing.expect(err2 == errors.RuntimeError.OtherError);
}
