const std = @import("std");
const wasm_core = @import("wasm-core");

/// Represents a complete .wast script file
pub const WastScript = struct {
    allocator: std.mem.Allocator,
    commands: std.ArrayList(Command),

    pub fn init(allocator: std.mem.Allocator) WastScript {
        return .{
            .allocator = allocator,
            .commands = .{},
        };
    }

    pub fn deinit(self: *WastScript) void {
        for (self.commands.items) |*cmd| {
            cmd.deinit(self.allocator);
        }
        self.commands.deinit(self.allocator);
    }

    pub fn addCommand(self: *WastScript, command: Command) !void {
        try self.commands.append(self.allocator, command);
    }
};

/// A command in a .wast file
pub const Command = union(enum) {
    module: ModuleCommand,
    assert_return: AssertReturn,
    assert_trap: AssertTrap,
    assert_invalid: AssertInvalid,
    register: Register,

    pub fn deinit(self: *Command, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .module => |*m| m.deinit(),
            .assert_return => |*a| a.deinit(allocator),
            .assert_trap => |*a| a.deinit(allocator),
            .assert_invalid => |*a| a.deinit(allocator),
            .register => |*r| r.deinit(allocator),
        }
    }
};

/// Module definition
pub const ModuleCommand = struct {
    name: ?[]const u8,
    module: wasm_core.types.Module,

    pub fn deinit(self: *ModuleCommand) void {
        self.module.deinit();
    }
};

/// assert_return assertion
pub const AssertReturn = struct {
    action: Action,
    expected: []const Value,

    pub fn deinit(self: *AssertReturn, allocator: std.mem.Allocator) void {
        self.action.deinit(allocator);
        allocator.free(self.expected);
    }

    pub fn format(self: @This(), writer: anytype) !void {
        try writer.writeAll("assert_return (");
        try self.action.format(writer);
        if (self.expected.len > 0) {
            try writer.writeAll(") => (");
            for (self.expected, 0..) |exp, i| {
                if (i > 0) try writer.writeAll(", ");
                try exp.format(writer);
            }
            try writer.writeAll(")");
        } else {
            try writer.writeAll(")");
        }
    }
};

/// assert_trap assertion
pub const AssertTrap = struct {
    action: Action,
    failure: []const u8,

    pub fn deinit(self: *AssertTrap, allocator: std.mem.Allocator) void {
        self.action.deinit(allocator);
        allocator.free(self.failure);
    }
};

/// assert_invalid assertion
pub const AssertInvalid = struct {
    module_text: []const u8,
    failure: []const u8,

    pub fn deinit(self: *AssertInvalid, allocator: std.mem.Allocator) void {
        allocator.free(self.module_text);
        allocator.free(self.failure);
    }
};

/// register command
pub const Register = struct {
    name: []const u8,
    module_name: ?[]const u8,

    pub fn deinit(self: *Register, allocator: std.mem.Allocator) void {
        allocator.free(self.name);
        if (self.module_name) |name| {
            allocator.free(name);
        }
    }
};

/// An action (invoke or get)
pub const Action = union(enum) {
    invoke: Invoke,
    get: Get,

    pub fn deinit(self: *Action, allocator: std.mem.Allocator) void {
        switch (self.*) {
            .invoke => |*i| i.deinit(allocator),
            .get => |*g| g.deinit(allocator),
        }
    }

    pub fn format(self: @This(), writer: anytype) !void {
        switch (self) {
            .invoke => |inv| try inv.format(writer),
            .get => |g| try g.format(writer),
        }
    }
};

/// invoke action
pub const Invoke = struct {
    module_name: ?[]const u8,
    func_name: []const u8,
    args: []const Value,

    pub fn deinit(self: *Invoke, allocator: std.mem.Allocator) void {
        if (self.module_name) |name| {
            allocator.free(name);
        }
        allocator.free(self.func_name);
        allocator.free(self.args);
    }

    pub fn format(self: @This(), writer: anytype) !void {
        try writer.writeAll("invoke");
        if (self.module_name) |module| {
            try writer.print(" ${s}", .{module});
        }
        try writer.print(" \"{s}\"", .{self.func_name});
        if (self.args.len > 0) {
            try writer.writeAll(" (");
            for (self.args, 0..) |arg, i| {
                if (i > 0) try writer.writeAll(", ");
                try arg.format(writer);
            }
            try writer.writeAll(")");
        }
    }
};

/// get action
pub const Get = struct {
    module_name: ?[]const u8,
    global_name: []const u8,

    pub fn deinit(self: *Get, allocator: std.mem.Allocator) void {
        if (self.module_name) |name| {
            allocator.free(name);
        }
        allocator.free(self.global_name);
    }

    pub fn format(self: @This(), writer: anytype) !void {
        try writer.writeAll("get");
        if (self.module_name) |module| {
            try writer.print(" ${s}", .{module});
        }
        try writer.print(" \"{s}\"", .{self.global_name});
    }
};

/// A value (used in assertions and invocations)
pub const Value = union(enum) {
    i32: i32,
    i64: i64,
    f32: f32,
    f64: f64,
    v128: i128,
    ref_null: wasm_core.types.RefType,
    ref_extern: u32,
    ref_func: u32,

    pub fn format(self: @This(), writer: anytype) !void {
        switch (self) {
            .i32 => |v| try writer.print("i32.const {d}", .{v}),
            .i64 => |v| try writer.print("i64.const {d}", .{v}),
            .f32 => |v| try writer.print("f32.const {d}", .{v}),
            .f64 => |v| try writer.print("f64.const {d}", .{v}),
            .v128 => |v| try writer.print("v128.const {d}", .{v}),
            .ref_null => |t| try writer.print("ref.null {any}", .{t}),
            .ref_extern => |v| try writer.print("ref.extern {d}", .{v}),
            .ref_func => |v| try writer.print("ref.func {d}", .{v}),
        }
    }
};
