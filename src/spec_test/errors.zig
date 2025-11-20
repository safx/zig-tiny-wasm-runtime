// Re-export from spec-test-errors module
const spec_test_errors = @import("spec-test-errors");

pub const DecodeError = spec_test_errors.DecodeError;
pub const ValidationError = spec_test_errors.ValidationError;
pub const RuntimeError = spec_test_errors.RuntimeError;

pub const decodeErrorFromString = spec_test_errors.decodeErrorFromString;
pub const validationErrorFromString = spec_test_errors.validationErrorFromString;
pub const linkErrorFromString = spec_test_errors.linkErrorFromString;
pub const runtimeErrorFromString = spec_test_errors.runtimeErrorFromString;
