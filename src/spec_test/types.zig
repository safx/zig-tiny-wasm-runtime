const std = @import("std");
const decode = @import("wasm-decode");
const runtime = @import("wasm-runtime");
pub const Error = decode.Error || runtime.Error;

// https://github.com/WebAssembly/spec/tree/master/interpreter#scripts

pub const Command = union(enum) {
    action: ActionCommandArg,

    // module
    module: ModuleCommandArg,
    module_quote,

    register: RegisterCommandArg,

    // assertion
    assert_return: AssertReturnCommandArg,
    assert_trap: AssertTrapCommandArg,
    assert_exhaustion,
    assert_malformed,
    assert_invalid,
    assert_unlinkable,
    assert_uninstantiable,
};

pub const Action = union(enum) {
    invoke: InvokeCommandArg,
    get: GetCommandArg,
};

pub const Result = union(enum) {
    @"const": runtime.Value,
    //
    f32_nan_canonical,
    f32_nan_arithmetic,
    f64_nan_canonical,
    f64_nan_arithmetic,
};

pub const ActionCommandArg = struct {
    line: u32,
    action: Action,
};

pub const ModuleCommandArg = struct {
    line: u32,
    file_name: []const u8,
    name: ?[]const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (self.name) |n| {
            _ = try writer.print("{s} (-> {s}) (line:{})", .{ self.file_name, n, self.line });
        } else {
            _ = try writer.print("{s} (line:{})", .{ self.file_name, self.line });
        }
    }
};

pub const RegisterCommandArg = struct {
    as_name: []const u8,
    name: ?[]const u8,
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
    args: []const runtime.Value,
    module: ?[]const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (self.module) |m| {
            _ = try writer.print("{s} {any} (module:{s})", .{ self.field, self.args, m });
        } else {
            _ = try writer.print("{s} {any}", .{ self.field, self.args });
        }
    }
};

pub const GetCommandArg = struct {
    field: []const u8,
    module: ?[]const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s})", .{self.field});
    }
};
