pub const errors = @import("./errors.zig");

// Re-export error types
pub const DecodeError = errors.DecodeError;
pub const ValidationError = errors.ValidationError;
pub const RuntimeError = errors.RuntimeError;

// Re-export conversion functions
pub const decodeErrorFromString = errors.decodeErrorFromString;
pub const validationErrorFromString = errors.validationErrorFromString;
pub const linkErrorFromString = errors.linkErrorFromString;
pub const runtimeErrorFromString = errors.runtimeErrorFromString;

test {
    _ = @import("./test.zig");
}
