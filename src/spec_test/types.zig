const std = @import("std");
const decode = @import("wasm-decode");
const runtime = @import("wasm-runtime");
const errors = @import("./errors.zig");

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
    assert_exhaustion: AssertExhaustionCommandArg,
    assert_malformed: AssertMalformedCommandArg,
    assert_invalid: AssertInvalidCommandArg,
    assert_unlinkable: AssertUnlinkableCommandArg,
    assert_uninstantiable: AssertUninstantiableCommandArg,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            inline else => |val| if (@TypeOf(val) == void) {
                try writer.print("{s}", .{@tagName(self)});
            } else {
                try writer.print("{s}: {any}", .{ @tagName(self), val });
            },
        }
    }
};

pub const Action = union(enum) {
    invoke: InvokeCommandArg,
    get: GetCommandArg,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            inline else => |val| {
                try writer.print("{s}: {any}", .{ @tagName(self), val });
            },
        }
    }
};

pub const ResultOld = union(enum) {
    @"const": runtime.Value,
    //
    f32_nan_canonical,
    f32_nan_arithmetic,
    f64_nan_canonical,
    f64_nan_arithmetic,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        switch (self) {
            .@"const" => |val| try writer.print("{any}", .{val}),
            inline else => try writer.print("{s}", .{@tagName(self)}),
        }
    }
};

pub const Result = union(enum) {
    const Self = @This();
    i32: i32,
    i64: i64,
    f32: FloatType(u32),
    f64: FloatType(u64),

    func_ref: ?runtime.FuncAddr,
    extern_ref: ?runtime.ExternAddr,

    v128: i128, // normal vector

    // vector including nan
    vec_f32: [4]FloatType(u32),
    vec_f64: [2]FloatType(u64),
};

pub fn FloatType(comptime T: type) type {
    return union(enum) {
        value: T,
        nan_canonical,
        nan_arithmetic,
    };
}

pub const ActionCommandArg = struct {
    line: u32,
    action: Action,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{any} (line:{})", .{ self.action, self.line });
    }
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

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        if (self.name) |n| {
            _ = try writer.print("{s} {s}", .{ self.as_name, n });
        } else {
            _ = try writer.print("{s}", .{self.as_name});
        }
    }
};

pub const AssertReturnCommandArg = struct {
    line: u32,
    action: Action,
    expected: []const Result,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{any} (-> {any}) (line:{})", .{ self.action, self.expected, self.line });
    }
};

pub const AssertTrapCommandArg = struct {
    line: u32,
    action: Action,
    trap: errors.RuntimeError,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{any} (-> {}) (line:{})", .{ self.action, self.trap, self.line });
    }
};

pub const AssertExhaustionCommandArg = struct {
    line: u32,
    action: Action,
    trap: errors.RuntimeError,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{any} (-> {}) (line:{})", .{ self.action, self.trap, self.line });
    }
};

pub const AssertMalformedCommandArg = struct {
    line: u32,
    file_name: []const u8,
    trap: errors.DecodeError,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s} (-> {}) (line:{})", .{ self.file_name, self.trap, self.line });
    }
};

pub const AssertInvalidCommandArg = struct {
    line: u32,
    file_name: []const u8,
    trap: errors.ValidationError,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s} (-> {}) (line:{})", .{ self.file_name, self.trap, self.line });
    }
};

pub const AssertUnlinkableCommandArg = struct {
    line: u32,
    file_name: []const u8,
    trap: errors.RuntimeError,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s} (-> {}) (line:{})", .{ self.file_name, self.trap, self.line });
    }
};

pub const AssertUninstantiableCommandArg = struct {
    line: u32,
    file_name: []const u8,
    trap: errors.RuntimeError,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s} (-> {}) (line:{})", .{ self.file_name, self.trap, self.line });
    }
};

pub const InvokeCommandArg = struct {
    field: []const u8,
    args: []const runtime.Value,
    module: ?[]const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s}", .{self.field});

        if (self.args.len > 0) {
            _ = try writer.print(" {any}", .{self.args});
        }

        if (self.module) |m| {
            _ = try writer.print(" (module:{s})", .{m});
        }
    }
};

pub const GetCommandArg = struct {
    field: []const u8,
    module: ?[]const u8,

    pub fn format(self: @This(), comptime _: []const u8, _: std.fmt.FormatOptions, writer: anytype) !void {
        _ = try writer.print("{s}", .{self.field});
    }
};
