const decode = @import("wasm-decode");
const runtime = @import("wasm-runtime");
pub const Error = decode.Error || runtime.Error;

// https://github.com/WebAssembly/spec/tree/master/interpreter#scripts

pub const Command = union(enum) {
    // module
    module: ModuleCommandArg,
    module_quote,

    register,

    // assertion
    assert_return: AssertReturnCommandArg,
    assert_trap: AssertTrapCommandArg,
    assert_exhaustion,
    assert_malformed,
    assert_invalid,
    assert_unlinkable,
};

pub const Action = union(enum) {
    invoke: InvokeCommandArg,
    get,
};

pub const Const = runtime.Value;

pub const Result = union(enum) {
    @"const": Const,
    //
    f32_nan_canonical,
    f32_nan_arithmetic,
    f64_nan_canonical,
    f64_nan_arithmetic,
};

pub const ModuleCommandArg = struct {
    line: u32,
    file_name: []const u8,
};

pub const AssertReturnCommandArg = struct {
    line: u32,
    action: Action,
    expected: []const Result,
};

pub const AssertTrapCommandArg = struct {
    line: u32,
    action: Action,
    trap: Error,
};

pub const InvokeCommandArg = struct {
    field: []const u8,
    args: []const Const,
};

pub const ConstNumber = struct {
    type: []const u8,
    value: []const u8,
};

pub const ActionType = enum {
    invoke,
};
